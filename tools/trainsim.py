#!/usr/bin/env python3

"""
For testing https://github.com/channable/hoff/issues/77#issuecomment-1179430191
"""

from __future__ import annotations

import math
import matplotlib  # type: ignore
import numpy as np
import heapq

from collections import defaultdict
from textwrap import dedent
from dataclasses import dataclass
from matplotlib import pyplot as plt
from matplotlib.font_manager import FontProperties  # type: ignore
from matplotlib.ticker import MultipleLocator  # type: ignore
from numpy.random import Generator
from numpy.typing import ArrayLike
from typing import Callable, Iterable, NamedTuple, NewType, Optional, Tuple

Time = NewType("Time", float)
Commit = NewType("Commit", int)
PrId = NewType("PrId", int)
BuildId = NewType("BuildId", int)

# When we set the mean build time to the average time between PRs, we are at
# that critical point where on average the system can keep up and still merge
# everything, but there will be spans of time where we are unlucky and a backlog
# builds up. Being *right* at that critical point is bad because once there is
# some backlog, it has as much probability of growing as it has of shrinking,
# but it can't shrink below zero, so over time it does grow. So let's say we are
# not yet at that point, and the time between PRs is a bit more than the build
# time.
class Config(NamedTuple):
    avg_time_between_prs: Time
    avg_time_to_approve: Time = Time(60.0)
    avg_build_time: Time = Time(10.0)
    build_time_stdev: Time = Time(1.0)
    # When generating PRs, the probability that one is *actually* good.
    probability_pr_is_good: float = 0.85
    # For the Bayesian update that the state keeps of the probability that a
    # given PR is good, the initial probability, which does not have to match
    # the real probability defined above.
    prior_is_good_probability: float = 0.85
    num_prs: int = 250
    num_build_slots: int = 4

    @staticmethod
    def new(parallelism: int, criticality: float) -> Config:
        """
        Criticality has three regimes:

        Subcritical.
        In this case, the average time between PRs is greater than the build
        time, so the system should be able to keep up, and have an empty queue
        much of the time. There might be an occasional backlog, but we can
        process it quickly.

        Critical.
        In this case, the average time between PRs is equal to the build time.
        This means that rate of the "producer" and "consumer" are matched, so
        on average we can keep up. But in this regime, when a backlog is there
        because we were unlucky for some time, on average we don't clear it.
        And when we are lucky for a span of time, the backlog size can't fall
        below zero. So over time, the backlog still grows!

        Supercritical.
        In this case, the average time between PRs is smaller than the build
        time, so any strategy without parallelism or rollups will not be able
        to cope and cause an ever-growing backlog.
        """
        return Config(
            avg_time_between_prs=Time(10.0 / (criticality * parallelism)),
            num_build_slots=parallelism,
        )


class PullRequest(NamedTuple):
    arrived_at: Time
    id_: PrId
    is_good: bool


class BuildResult(NamedTuple):
    arrived_at: Time
    id_: BuildId
    did_succeed: bool


def generate_pr_events(rng: Generator, config: Config) -> list[PullRequest]:
    """
    Generate arrival events for pull requests, in no particular order.
    """

    # We model incoming PRs as a Poisson process, which means the inter-arrival
    # times follow an exponential distribution.
    times_between_open = rng.exponential(
        scale=config.avg_time_between_prs, size=config.num_prs
    )

    # For the time to approve, let's assume that to approve a PR, three things
    # need to happen for which the delays all follow an exponential
    # distribution: the approver needs to see the review request notification,
    # the reviewer needs to complete some unrelated task, and then the reviewer
    # needs to do a review. We end up with a Gamma distribution with k=3, which
    # starts to look like a bell curve but is still skewed towards zero, while
    # putting 0 probability mass at 0 itself. So PRs take *some* nonzero time
    # to be approved, but the longer it takes, the less likely that is.
    times_to_approve = rng.gamma(
        shape=3,
        scale=config.avg_time_to_approve / 3,
        size=config.num_prs,
    )
    is_good = rng.uniform(size=config.num_prs) < config.probability_pr_is_good

    t_open = np.cumsum(times_between_open)
    t_approve = t_open + times_to_approve

    return [
        PullRequest(
            arrived_at=Time(t_approve[i]),
            id_=PrId(i),
            is_good=is_good[i],
        )
        for i in range(0, config.num_prs)
    ]


class Build(NamedTuple):
    # Commit that this train was built on top of.
    base: Commit

    # Last commit in this train, which would become the new tip of the master
    # branch if we merged this train.
    tip: Commit

    # For a chain of builds, each on top of one another, ending at the current
    # build, this lists all pull requests included in base..tip. For example,
    # if this build is a train directly on top of master, it would contain one
    # entry {master: prs} with all PRs in this pull request. If this build
    # instead is on top of another build A, then we would have
    # {A.base: self.prs | A.prs, A.tip: self.prs}. This dict should be ordered
    # with the root first, and builds on top later.
    root_path: dict[Commit, set[PrId]]

    # Ids of the pull requests included in base..tip.
    prs: set[PrId]

    def get_root(self) -> Commit:
        """
        Return the latest known-good commit that this build builds on top of.
        """
        root, _prs_since_root = next(iter(self.root_path.items()))
        return root

    def prs_since_root(self) -> set[PrId]:
        _root, prs_since_root = next(iter(self.root_path.items()))
        return prs_since_root

    def trim_root_path(self, new_tip: Commit) -> Build:
        """
        When we advance master to `new_tip`, that affects the root path of all
        PRs that have `new_tip` on their path. This trims the root path to
        remove everything before `new_tip`.
        """
        if new_tip not in self.root_path:
            return self

        still_pending_prs = self.root_path[new_tip]
        new_root_path = {
            base: prs
            for base, prs in self.root_path.items()
            # As we walk from the root to the tip, the next set of PRs we
            # encounter should be a strict subset of the current one, which
            # means we can figure out where to make the cut based on just the
            # size of the set.
            if len(prs) <= len(still_pending_prs)
        }
        return self._replace(root_path=new_root_path)


class Speculation(NamedTuple):
    """
    The result of speculating what would happen if we would build a particular
    subset of pull requests from a probability distribution.
    """
    # The probability that the subset will succeed to build.
    p_success: float
    # The entropy of the distribution after observing success for this subset.
    entropy_success: float
    # The entropy of the distribution after observing failure for this subset.
    entropy_failure: float
    # The number of pull requests that we know are certainly good or bad, after
    # observing success for this subset.
    n_confirmed_success: float
    # The number of pull requests that we know are certainly good or bad, after
    # observing failure for this subset.
    n_confirmed_failure: float

    def expected_wait_time(self, queue_size: float) -> float:
        """
        Return the expected remaining total wait time after the result of
        building this pull request comes in.
        """
        # The entropy of the resulting distribution is a rough estimate of how
        # many builds we still have to do, and the size of the queue minus what
        # we reduce it by, is the number of PRs that incur that wait time.
        # Technically we should expect it to be more like the surface area of a
        # triangle and add a factor 0.5 in there, but the constant wouldn't
        # doesn't change the ranking.
        len_success = (queue_size - self.n_confirmed_success) * self.entropy_success
        len_failure = (queue_size - self.n_confirmed_failure) * self.entropy_failure
        return self.p_success * len_success + (1.0 - self.p_success) * len_failure

    def __repr__(self) -> str:
        return (
            f"p_success={self.p_success:.3f} "
            f"entropy_success={self.entropy_success:.3f} "
            f"entropy_failure={self.entropy_failure:.3f} "
            f"n_confirmed_success={self.n_confirmed_success:.3f} "
            f"n_confirmed_failure={self.n_confirmed_failure:.3f}"
        )


class ProbDist(NamedTuple):
    """
    A probability distribution over {good, bad}^n the set of all possible states
    of n pull requests.

    The states are numbered in binary, where the least significant bit is 0 for
    a state where `open_prs[0]` is bad, and it is 1 for a state where
    `open_prs[0]` is good, with the more significant bit for higher indexes.
    """
    prs: list[PrId]
    ps: list[float]

    @staticmethod
    def new() -> ProbDist:
        return ProbDist([], [1.0])

    def insert(self, id_: PrId, p_is_good: float) -> ProbDist:
        assert len(self.prs) < 25, "This would quickly make your machine go OOM."
        p_is_bad = 1.0 - p_is_good
        return ProbDist(
            prs=self.prs + [id_],
            ps=(
                # The first n values are when pr "id_" fails,
                # the next n when it passes.
                [p_is_bad * p for p in self.ps] +
                [p_is_good * p for p in self.ps]
            ),
        )

    def remove(self, id_: PrId) -> ProbDist:
        i = self.prs.index(id_)
        mask_i = 1 << i
        mask_low = mask_i - 1
        mask_high = len(self.ps) - 1 - mask_low

        ps = []
        for k in range(len(self.ps) // 2):
            # For one element in the new distribution, we had two previously:
            # one where PR i was bad, and one where it was good. Sum those.
            old_k = ((k & mask_high) << 1) | (k & mask_low)
            ps.append(self.ps[old_k] + self.ps[old_k + mask_i])

        return ProbDist(
            prs=self.prs[:i] + self.prs[i + 1:],
            ps=ps,
        )

    def pack(self, ids: set[PrId]) -> int:
        """
        Pack the set of pull requests into a bit mask that we can use for
        indexing into this distribution.
        """
        return sum(1 << i for i, pr in enumerate(self.prs) if pr in ids)

    def unpack(self, mask: int) -> set[PrId]:
        """
        Expand a bitmask into the PRs selected by it.
        """
        return { pr for i, pr in enumerate(self.prs) if (1 << i) & mask > 0 }

    def observe_outcome(self, ids: set[PrId], is_good: bool) -> ProbDist:
        """
        Perform a Bayesian update, for the evidence that the set of pull
        requests built together was good/bad.
        """
        mask = self.pack(ids)

        if mask == 0:
            return self

        n = len(self.ps)
        ps: list[float] = []

        if is_good:
            p_outcome = sum(self.ps[i] for i in range(n) if i & mask == mask)
        else:
            p_outcome = sum(self.ps[i] for i in range(n) if i & mask != mask)

        assert p_outcome > 0.0, "Cannot observe an impossible outcome."

        # You can enable this print to inspect the update.
        # print(f"{mask=:05b} {p_outcome=:.4f}")

        for k in range(n):
            # Apply Bayes' rule for the probability that we are in world k.
            if is_good:
                p_outcome_given_k = float(k & mask == mask)
            else:
                p_outcome_given_k = float(k & mask != mask)

            p_k = self.ps[k]
            p_k_given_outcome = (p_outcome_given_k * p_k) / p_outcome
            # You can enable this print to inspect the update.
            # print(
            #     f"{k=:05b} {p_k=:.4f} {p_outcome_given_k=:.4f} "
            #     f"{p_k_given_outcome=:.4f}"
            # )

            ps.append(p_k_given_outcome)

        return ProbDist(self.prs, ps)

    def __repr__(self) -> str:
        result = [f"ProbDist(prs={self.prs}, entropy={self.entropy():.2f}, ps=["]
        w = (len(self.ps) - 1).bit_length()
        ps = [f"{k:b}".zfill(w) + f" {p:.4f}" for k, p in enumerate(self.ps)]
        nrows = max(2, int(math.sqrt(len(ps))) - 1)
        while len(ps) > nrows:
            m = len(ps) // 2
            ps = [ps[i] + "  " + ps[m + i] for i in range(m)]

        result.extend("  " + line for line in ps)
        result.append("])")
        return "\n".join(result)

    def entropy(self) -> float:
        """Return the Shannon entropy of the distribution."""
        # Throw in a max to avoid negative zero, because it looks ugly.
        return max(0.0, -sum(p * math.log2(p) for p in self.ps if p > 0.0))

    def flatten(self) -> dict[PrId, float]:
        """
        For each PR, return the probability that it is good. Note, this flattens
        the distribution; the probabilities for some PRs may be correlated, and
        that information is lost here.
        """
        ps = [0.0 for _ in self.prs]
        n = len(self.ps)
        m = len(self.prs)
        for k, p in enumerate(self.ps):
            for i in range(m):
                if (1 << i) & k > 0:
                    ps[i] += p

        return {pr: p for pr, p in zip(self.prs, ps)}

    def subset_probabilities(self) -> ProbDist:
        """
        For every possible subset of the set of pull requests, return the
        probability that building that subset would succeed.
        """
        # The success probability for subset of PRs, where every bit in an
        # element's index determines whether the corresponding PR is part of the
        # set. (So 0 is the empty set, n-1 is the full set.)
        ps = [0.0 for _ in self.ps]
        n = len(self.ps)
        for k, p in enumerate(self.ps):
            for s in range(n):
                if k & s == s:
                    ps[s] += p

        return ProbDist(prs=self.prs, ps=ps)

    def speculate_subsets(self) -> Iterable[Speculation]:
        """
        For every possible subset of the set of pull requests, inspect both the
        cases where it would pass if we built it, and where it would fail, and
        return some info about that.

        The result is a list where the binary representation of the index of an
        element decides which pull requests to include. E.g. at index 0 is the
        empty set, at index len-1 is the full set itself.
        """
        # In addition to the probability per state, get the subset
        # probabilities.
        subset_probabilities = self.subset_probabilities()
        ps = subset_probabilities.ps
        n = len(ps)
        mask = n - 1

        # Now we kind of do the Bayesian update for every subset, except we
        # don't store the new probabilities, we store the expected entropy.
        for s, p_good in enumerate(ps):
            p_bad = 1.0 - p_good
            mask_good_good = mask
            mask_good_bad = mask
            mask_bad_good = mask
            mask_bad_bad = mask
            entropy_success = 0.0
            entropy_failure = 0.0

            for k, p_k in enumerate(self.ps):
                p_good_given_k = float(k & s == s)
                p_bad_given_k = float(k & s != s)

                p_k_given_good = (p_good_given_k * p_k) / p_good if p_good > 0.0 else 0.0
                p_k_given_bad = (p_bad_given_k * p_k) / p_bad if p_bad > 0.0 else 0.0

                entropy_success -= p_k_given_good * math.log2(p_k_given_good) if p_k_given_good > 0.0 else 0.0
                entropy_failure -= p_k_given_bad * math.log2(p_k_given_bad) if p_k_given_bad > 0.0 else 0.0

                # Update the masks for which pull requests can be good or bad,
                # we count this at the same time.
                if p_k_given_good > 0.0:
                    mask_good_good &= k
                    mask_bad_good &= mask - k
                if p_k_given_bad > 0.0:
                    mask_good_bad &= k
                    mask_bad_bad &= mask - k

            yield Speculation(
                p_success=p_good,
                # These maxes are only really here to avoid negative zeros,
                # which are not harmful but look ugly.
                entropy_success=max(0.0, entropy_success),
                entropy_failure=max(0.0, entropy_failure),
                n_confirmed_success=(mask_good_good | mask_bad_good).bit_count(),
                n_confirmed_failure=(mask_good_bad | mask_bad_bad).bit_count(),
            )

    def prs_confirmed(self) -> Tuple[set[PrId], set[PrId]]:
        """
        Return the sets of (good prs, bad prs) which are 100% certain to be good
        or bad based on past observations.
        """
        n = len(self.prs)
        mask = (1 << n) - 1
        mask_good = mask
        mask_bad = mask
        for k, p in enumerate(self.ps):
            if p > 0.0:
                mask_good &= k
                mask_bad &= mask - k

        return self.unpack(mask_good), self.unpack(mask_bad)


class State(NamedTuple):
    open_prs: dict[PrId, PullRequest]

    # Per merged or failed PR, the time from arrival until it got merged.
    closed_prs: dict[PrId, Time]

    # The probability distribution over all possible states in {good, bad}^n of
    # the n open pull requests.
    pd: ProbDist

    builds_in_progress: dict[BuildId, Build]

    # The successive commits that the master branch pointed to.
    heads: list[Commit]

    num_build_slots: int
    prior_is_good_probability: float

    @staticmethod
    def new(prior_is_good_probability: float, num_build_slots: int) -> State:
        return State(
            open_prs={},
            closed_prs={},
            pd=ProbDist.new(),
            builds_in_progress={},
            heads=[Commit(0)],
            num_build_slots=num_build_slots,
            prior_is_good_probability=prior_is_good_probability,
        )

    def get_tip(self) -> Commit:
        """Return the current tip of the master branch."""
        return self.heads[-1]

    def probabilities_good(self) -> dict[PrId, float]:
        """
        Return the probability that the pull request is good for all tracked
        pull requests.
        """
        return self.pd.flatten()

    def insert_pr(self, pr: PullRequest) -> State:
        return self._replace(open_prs=self.open_prs | {pr.id_: pr})._refresh_pd()

    def _refresh_pd(self) -> State:
        """
        Remove any pull requests from the probability distribution that are no
        longer open, and add more PRs is there is space for that.

        The memory used by the probability distribution is exponential in its
        size, and many operations are too, so if it grows too big, it becomes
        intractable. To make it manageable, we only track probabilities for some
        maximum number of PRs. When we get more they still go in the backlog,
        but they just sit there (and therefore we never consider them for
        merging) until there is space for them in the probability distribution.
        """
        # First, remove any PRs that are no longer open, and no longer being
        # built. (If there is still a build for it in progress, we need to keep
        # it, otherwise we might falsely blame a failure of such build on the
        # remaining PRs, when we already removed the real culprit.)
        in_progress_prs: set[PrId] = set()
        for build in self.builds_in_progress.values():
            in_progress_prs = in_progress_prs | build.prs_since_root()

        new_pd = self.pd
        for pr_id in self.pd.prs:
            if pr_id not in self.open_prs and pr_id not in in_progress_prs:
                new_pd = new_pd.remove(pr_id)

        # Then, if there is space, we can add PRs to the probability
        # distribution. If there are many pull requests waiting to enter the PD,
        # add the one with the lowest id. NOTE: This creates a kind of "Classic"
        # strategy again when the backlog is big. You can experiment with the
        # fifo/lifo alternatives here too.
        # After some testing, with a capacity of 7, the simulation can still run
        # in a reasonable time (~30s per run on my machine). You can crank it up
        # to possibly get somewhat better results at the cost of being very
        # slow.
        spots_left = 7 - len(new_pd.prs)
        if spots_left > 0:
            prs_not_in_pd = sorted(set(self.open_prs.keys()) - set(self.pd.prs))
            for pr_id in prs_not_in_pd[:spots_left]:
                new_pd = new_pd.insert(pr_id, p_is_good=self.prior_is_good_probability)

        return self._replace(pd=new_pd)

    def start_build(self, id_: BuildId, build: Build) -> State:
        return self._replace(builds_in_progress=self.builds_in_progress | {id_: build})

    def complete_build_success(self, t: Time, id_: BuildId) -> State:
        """
        Complete the build, advance the tip of the master branch if applicable,
        remove any merged PRs from the state, and record the time elapsed since
        the PR arrived for all merged PRs, in addition to the new state.
        """
        build = self.builds_in_progress[id_]
        new_builds = {
            bid: b for bid, b in self.builds_in_progress.items() if bid != id_
        }

        good_prs = build.prs_since_root()
        new_pd = self.pd.observe_outcome(build.prs_since_root(), is_good=True)

        new_open_prs = dict(self.open_prs.items())
        new_closed_prs = dict(self.closed_prs.items())

        def close_pr(pr_id: PrId) -> None:
            pr = new_open_prs[pr_id]
            del new_open_prs[pr_id]
            dt = Time(t - pr.arrived_at)
            assert dt > Time(0.0)
            assert pr_id not in new_closed_prs
            new_closed_prs[pr_id] = dt

        _confirmed_good, confirmed_bad = new_pd.prs_confirmed()
        for pr_id in confirmed_bad:
            # We might confirm a PR to be bad that is no longer open, because
            # pull requests need to remain in the probability distribution while
            # there are builds for them in progress.
            if pr_id in new_open_prs:
                print(f"Confirmed bad indirectly: {pr_id}")
                close_pr(pr_id)

        tip = self.get_tip()
        if tip not in build.root_path:
            # This success was not on top of master, so it was not directly
            # useful, but we may still have made progress.
            return self._replace(
                open_prs=new_open_prs,
                closed_prs=new_closed_prs,
                pd=new_pd,
                builds_in_progress=new_builds,
            )._refresh_pd()

        # If we merge the tip of this build, then that means that some other
        # builds that were previously speculative, might now be directly on top
        # of master, so update their root paths.
        new_builds = {
            bid: b.trim_root_path(new_tip=build.tip) for bid, b in new_builds.items()
        }

        prs_since_tip = build.root_path[tip]
        new_heads = self.heads + [build.tip]

        print(f"Merge: new_tip={build.tip} {prs_since_tip=}")

        for pr_id in prs_since_tip:
            close_pr(pr_id)

        return self._replace(
            open_prs=new_open_prs,
            closed_prs=new_closed_prs,
            pd=new_pd,
            builds_in_progress=new_builds,
            heads=new_heads,
        )._refresh_pd()

    def complete_build_failure(self, t: Time, id_: BuildId) -> State:
        """
        Complete the build, update the probabilities for the PRs involved, and
        if we can confirm something as certainly bad, mark that PR as failed.
        """
        build = self.builds_in_progress[id_]
        new_builds = {
            bid: b for bid, b in self.builds_in_progress.items() if bid != id_
        }

        # Find the minimal set of PRs that contains a bad one, based on what's
        # included in this build and what got merged. I.e. if this build failed,
        # but it was on top of another build, and that other build passed in the
        # meantime but now this one fails, we can say that the failure was in
        # our build. On the other hand, if the other build is still pending,
        # then the entire range since master is potentially bad.
        bad_prs: set[PrId] = set()
        was_rooted = False
        for head in reversed(self.heads):
            if head in build.root_path:
                bad_prs = build.root_path[head]
                was_rooted = True
                break

        assert was_rooted, "A build must eventually be traceable to a succeeding build."

        new_pd = self.pd.observe_outcome(bad_prs, is_good=False)
        new_open_prs = self.open_prs
        new_closed_prs = self.closed_prs

        prs_confirmed_good, prs_confirmed_bad = new_pd.prs_confirmed()
        # TODO: Sometimes I hit this assertion ... why? Can a build failure
        # actually confirm another PR to be good?
        # assert len(prs_confirmed_good) == 0, "Build failure can never prove a PR to be good."

        # If the build caused us to learn that any PRs are certainly bad, mark
        # those as failed.
        for pr_id in prs_confirmed_bad:
            if pr_id not in self.open_prs:
                # For some PRs, we might already know that they are bad, but we
                # need to keep them around in the PD because some builds
                # referencing them, so not all confirmed-bad PRs need to be open.
                assert pr_id in self.closed_prs
                continue

            pr = self.open_prs[pr_id]
            dt = Time(t - pr.arrived_at)
            assert dt > Time(0.0)
            new_open_prs = {k: v for k, v in self.open_prs.items() if k != pr_id}
            new_closed_prs = self.closed_prs | {pr_id: dt}

        return self._replace(
            open_prs=new_open_prs,
            closed_prs=new_closed_prs,
            pd=new_pd,
            builds_in_progress=new_builds,
        )._refresh_pd()


# A strategy is a function that, given the current state, proposes a new train
# to start a build for. The train is specified by the PRs to include, and the
# commit to build it on top of.
Strategy = Callable[[State], Tuple[Commit, set[PrId]]]


@dataclass(frozen=False)
class Simulator:
    config: Config
    strategy: Strategy
    rng: Generator
    t: Time
    state: State
    events: list[PullRequest | BuildResult]
    backlog_size_over_time: list[Tuple[Time, int]]
    next_available_commit: Commit
    next_available_build: BuildId

    @staticmethod
    def new(seed: int, config: Config, strategy: Strategy) -> Simulator:
        rng = np.random.default_rng(seed=seed)
        events: list[PullRequest | BuildResult] = [
            evt for evt in generate_pr_events(rng, config)
        ]
        heapq.heapify(events)
        return Simulator(
            config=config,
            strategy=strategy,
            rng=rng,
            t=Time(0.0),
            state=State.new(
                config.prior_is_good_probability,
                num_build_slots=config.num_build_slots,
            ),
            events=events,
            backlog_size_over_time=[],
            next_available_commit=Commit(1),
            next_available_build=BuildId(0),
        )

    def allocate_commit(self) -> Commit:
        result = self.next_available_commit
        self.next_available_commit = Commit(self.next_available_commit + 1)
        return result

    def allocate_build_id(self) -> BuildId:
        result = self.next_available_build
        self.next_available_build = BuildId(self.next_available_build + 1)
        return result

    def start_build_if_possible(self) -> bool:
        """
        Start a build if possible, return whether we started the build.
        """
        has_anything_to_build = len(self.state.open_prs) > 0
        has_capacity_to_build = (
            len(self.state.builds_in_progress) < self.config.num_build_slots
        )
        if not (has_anything_to_build and has_capacity_to_build):
            return False

        base, prs_in_train = self.strategy(self.state)

        if len(prs_in_train) == 0:
            # Some strategies might conclude they have nothing to do at some
            # point, even if there are open pull requests. For example, because
            # the pull request is already being built.
            return False

        assert all(
            pr in self.state.open_prs for pr in prs_in_train
        ), "Build must consist of currently open PRs."

        builds_by_tip = {
            other.tip: other for other in self.state.builds_in_progress.values()
        }
        root_path: dict[Commit, set[PrId]]
        if base in builds_by_tip:
            other = builds_by_tip[base]
            base_prs = other.prs_since_root()
            assert all(pr not in base_prs for pr in prs_in_train), (
                "Cannot include a pull request in a build on top of another "
                f"build {base}, when that other build already includes it. "
                f"{base_prs=} {prs_in_train=}"
            )
            root_path = {k: prs | prs_in_train for k, prs in other.root_path.items()}
            root_path[base] = prs_in_train
            assert other.get_root() == self.state.get_tip(), (
                "If we build atop another build, then that build must be on "
                "top of master, we shouldn't build on top of something that is "
                "already known to be obsolete."
            )

        elif base == self.state.get_tip():
            root_path = {base: prs_in_train}
        else:
            raise Exception("Must build on top of master or a currently running build.")

        assert (
            next(iter(root_path.keys())) == self.state.get_tip()
        ), "Build must directly or indirectly build upon the current tip."
        assert all(
            pr in self.state.open_prs for pr in next(iter(root_path.values()))
        ), "Build must build upon a train which is not guaranteed to fail."

        build = Build(
            base=base,
            tip=self.allocate_commit(),
            prs=prs_in_train,
            root_path=root_path,
        )
        build_id = self.allocate_build_id()

        print(
            f"Start build {build_id}: strategy={self.strategy} "
            f"master[-5:]={self.state.heads[-5:]} {build=}"
        )

        self.state = self.state.start_build(build_id, build)
        train_is_good = all(
            self.state.open_prs[pr].is_good for pr in build.prs_since_root()
        )
        build_duration = self.rng.normal(
            loc=self.config.avg_build_time, scale=self.config.build_time_stdev
        )
        completion = BuildResult(
            arrived_at=Time(self.t + build_duration),
            id_=build_id,
            did_succeed=train_is_good,
        )
        heapq.heappush(self.events, completion)
        return True

    def handle_single_event(self) -> None:
        event = heapq.heappop(self.events)
        self.t = event.arrived_at

        print(event, "size: ", len(self.state.open_prs))

        if isinstance(event, PullRequest):
            self.state = self.state.insert_pr(event)

        if isinstance(event, BuildResult):
            if event.did_succeed:
                self.state = self.state.complete_build_success(self.t, event.id_)
            else:
                self.state = self.state.complete_build_failure(self.t, event.id_)

        self.backlog_size_over_time.append((self.t, len(self.state.open_prs)))
        while self.start_build_if_possible():
            pass

    def run_to_last_pr(self) -> None:
        while len(self.events) > 0:
            self.handle_single_event()

            # We don't run until the last event is handled, we run until the
            # last PR is enqueued. Because we don't continue enqueueing PRs
            # forever, if we would wait for the last event, there is an atypical
            # period at the end where we clear the backlog, and we don't want
            # that to affect the delay statistics.
            prs_handled = len(self.state.open_prs) + len(self.state.closed_prs)
            if prs_handled == self.config.num_prs:
                break

    def get_backlog_trace(self, ts: ArrayLike) -> ArrayLike:
        """
        Sample the size of the backlog at the given times. This computes the
        time-weighted average size over the intervals defined by `ts`. The times
        `ts` must be increasing.
        """
        result: list[float] = []
        last_size = 0
        i = 0
        max_i = len(self.backlog_size_over_time)
        prev_t = 0.0

        for t in ts:  # type: ignore  # NumPy arrays are iterable just fine.
            assert isinstance(t, float)
            window_sum = 0.0
            weight = 0.0

            while i < max_i and self.backlog_size_over_time[i][0] < Time(t):
                elem_t = self.backlog_size_over_time[i][0]
                dt = elem_t - prev_t
                size = self.backlog_size_over_time[i][1]
                window_sum += dt * size
                weight += dt
                i += 1
                last_size = size

            if weight > 0.0:
                result.append(window_sum / weight)
            else:
                result.append(last_size)

        return np.array(result)

    def get_wait_times(self) -> ArrayLike:
        """
        Return wait times by PR id as a NumPy array.
        """
        return np.array([dt for pr_id, dt in sorted(self.state.closed_prs.items())])


def plot_results(config: Config, strategy_name: str, runs: list[Simulator]) -> None:
    font = FontProperties()
    font.set_family("Source Serif Pro")
    matplotlib.rcParams["font.family"] = font.get_name()

    fig, axes = plt.subplots(
        nrows=1,
        ncols=3,
        tight_layout=True,
        figsize=(15, 5),
        gridspec_kw={
            "width_ratios": [6, 6, 1],
        },
    )

    ax = axes[0]

    # We cut off the graph once the first few percent of runs has finished,
    # because when runs finish, there is no fresh data beyond that point for
    # those runs, so continuing to plot quantiles based on that would be
    # misleading.
    end_time = np.quantile([run.backlog_size_over_time[-1][0] for run in runs], 0.02)
    size_sample_times = np.linspace(0.0, end_time, num=200)

    backlog_sizes = np.array([run.get_backlog_trace(size_sample_times) for run in runs])
    # Even after we sample the backlog as a time-weighted value in the buckets,
    # the lines still look quite noisy, smooth that out a bit by averaging over
    # some time steps.
    window_len = 10
    tmp = np.cumsum(backlog_sizes, axis=1)
    tmp[:, window_len:] = tmp[:, window_len:] - tmp[:, :-window_len]
    backlog_sizes = tmp[:, window_len - 1 :] / window_len
    size_sample_times = size_sample_times[window_len - 1 :]

    # The scale of the timeline is somewhat arbitrary, and the build times
    # themselves never show up in absolute units. So let's normalize everything
    # to the average build time, then the timeline counts roughly "number of
    # builds".
    size_sample_times = size_sample_times / config.avg_build_time
    p10, p50, p90 = np.quantile(backlog_sizes, (0.1, 0.5, 0.9), axis=0)

    # Fit a line through the tail of the graph, so we can see if the queue size
    # is growing or steady.
    trend_index = len(size_sample_times) // 3
    fit_xs = np.tile(size_sample_times[trend_index:], backlog_sizes.shape[0])
    fit_ys = backlog_sizes[:, trend_index:].flatten()
    slope, intercept = np.polyfit(fit_xs, fit_ys, deg=1)
    trend = slope * size_sample_times[trend_index:] + intercept

    # Tweak the ticks a bit for some regions, NumPy's default have too little
    # detail to make the grid lines useful, in my opinion.
    if p90[-1] > 2.5:
        if p90[-1] < 8.0:
            ax.yaxis.set_major_locator(MultipleLocator(2.0))
            ax.yaxis.set_minor_locator(MultipleLocator(0.5))
        elif p90[-1] < 21.0:
            ax.yaxis.set_major_locator(MultipleLocator(5.0))
            ax.yaxis.set_minor_locator(MultipleLocator(1.0))
        elif p90[-1] < 31.0:
            ax.yaxis.set_major_locator(MultipleLocator(5.0))
            ax.yaxis.set_minor_locator(MultipleLocator(1.0))
        elif p90[-1] < 55.0:
            ax.yaxis.set_major_locator(MultipleLocator(10.0))
            ax.yaxis.set_minor_locator(MultipleLocator(2.0))
        elif p90[-1] < 130.0:
            ax.yaxis.set_major_locator(MultipleLocator(20.0))
            ax.yaxis.set_minor_locator(MultipleLocator(10.0))

    ax.grid(color="black", linestyle="dashed", axis="y", alpha=0.1, which="both")
    ax.fill_between(
        size_sample_times,
        p10,
        p90,
        alpha=0.2,
        color="black",
        label="backlog size, p10–p90",
    )
    ax.plot(
        size_sample_times[trend_index:],
        trend,
        color="red",
        alpha=0.5,
        linewidth=2.0,
        label="backlog size, linear fit on tail",
    )
    ax.plot(
        size_sample_times,
        p50,
        color="black",
        label="backlog size, p50",
    )
    ax.set_xlabel("time, in average build durations")
    ax.set_ylabel("number of approved pull requests not yet merged or failed")
    ax.legend(loc="upper left")
    ax.set_title("Backlog evolution")

    wait_times = np.concatenate([run.get_wait_times() for run in runs])
    # The scale of the wait times is somewhat arbitrary because it depends on
    # the build time we chose. So normalize everything to the average build
    # time, then we can express time waiting roughly in "number of builds".
    wait_times = wait_times / config.avg_build_time
    mean_wait_time = np.mean(wait_times)
    p50, p90, p98 = np.quantile(wait_times, (0.5, 0.9, 0.98))

    ax = axes[1]
    max_x = max(math.ceil(p98), 3)
    bins = np.arange(max_x * 2 + 1) * 0.5 - 0.25
    if max_x < 8:
        bins = np.arange(max_x * 4 + 1) * 0.25 - 0.125

    # Tweak NumPy's default ticks here too to make the grid more useful.
    if max_x <= 4:
        ax.xaxis.set_major_locator(MultipleLocator(0.5))
        ax.xaxis.set_minor_locator(MultipleLocator(0.25))
    elif max_x <= 11:
        ax.xaxis.set_major_locator(MultipleLocator(1.0))
        ax.xaxis.set_minor_locator(MultipleLocator(1.0))
    elif max_x <= 21:
        ax.xaxis.set_major_locator(MultipleLocator(2.0))
        ax.xaxis.set_minor_locator(MultipleLocator(1.0))
    elif max_x <= 31:
        ax.xaxis.set_major_locator(MultipleLocator(5.0))
        ax.xaxis.set_minor_locator(MultipleLocator(1.0))
    else:
        ax.xaxis.set_major_locator(MultipleLocator(10.0))
        ax.xaxis.set_minor_locator(MultipleLocator(2.0))

    ax.grid(color="black", linestyle="dashed", axis="x", alpha=0.1, which="both")
    ax.hist(wait_times, bins=bins, color="black", alpha=0.2)

    ax.axvline(
        x=mean_wait_time,
        color="red",
        label=f"wait time, mean ({mean_wait_time:.2f})",
    )
    p50, p90 = np.quantile(wait_times, (0.5, 0.9))
    ax.axvline(
        x=p50,
        color="black",
        label=f"wait time, p50 ({p50:.2f})",
    )
    ax.axvline(
        x=p90,
        color="black",
        linestyle="dotted",
        label=f"wait time, p90 ({p90:.2f})",
    )
    ax.set_xlabel("wait time until merge or fail, in average build durations")
    ax.set_ylabel("number of pull requests")
    ax.legend()
    ax.set_title("Wait time distribution")

    if slope > 0.02:
        ax.text(
            0.5,
            0.5,
            "Be careful when interpreting this histogram.\n"
            "It only shows wait times for pull requests that failed\n"
            "or got merged, but the backlog is growing over time.",
            transform=ax.transAxes,
            horizontalalignment="center",
            bbox={
                "boxstyle": "round",
                "facecolor": "white",
                "alpha": 0.8,
            },
        )

    ax = axes[2]
    prs_closed = np.array([len(run.state.closed_prs) for run in runs])
    prs_open = np.array([len(run.state.open_prs) for run in runs])
    completions = prs_closed / (prs_closed + prs_open)
    completion = np.mean(completions)
    bar = ax.bar(
        0,
        completion,
        width=0.45,
        color="black",
        alpha=0.2,
    )
    ax.bar_label(bar, labels=[f"{completion:0.3f}"], padding=-14)
    ax.set_xlim(-0.5, 0.5)
    ax.set_xticks([])

    if completion > 0.9:
        ax.set_ylim(0.9, 1.0)
        ax.set_yticks(np.linspace(0.9, 1.0, 11))
    elif completion > 0.5:
        ax.set_ylim(0.5, 1.0)
        ax.set_yticks(np.linspace(0.5, 1.0, 11))
    else:
        ax.set_ylim(0.0, 1.0)
        ax.set_yticks(np.linspace(0.0, 1.0, 11))

    ax.set_ylabel("fraction of pull requests merged or failed")
    ax.set_title("Completion")

    avg_time_between_prs = config.avg_time_between_prs / config.avg_build_time
    criticality = 1 / (config.num_build_slots * avg_time_between_prs)
    fig.suptitle(
        f"{strategy_name}, "
        f"{criticality:.1%} critical, "
        f"{config.num_build_slots} parallel builds, "
        f"{config.probability_pr_is_good:.0%} success rate"
    )
    plt.tight_layout()
    fname = (
        f"out/par{config.num_build_slots}_"
        f"crit{criticality:05.3f}_"
        f"succ{config.probability_pr_is_good:.2f}_"
        f"{strategy_name}.png"
    )
    plt.savefig(fname, dpi=400)


def strategy_optimistic_parallel(
    state: State, select: Callable[[set[PullRequest]], PrId]
) -> Tuple[Commit, set[PrId]]:
    """
    Meta-strategy for optimistic concurrency but without rollups. The selector
    should designate a signle PR to build, and this strategy will build them in
    that order, speculatively starting new builds on top of the existing train.
    """
    base = state.get_tip()
    candidates = set(state.open_prs.keys())

    # We build on top of the largest train that builds on top of the current
    # master. This assumes that the in-progress builds are ordered by "height"
    # on top of master, at least those that are grounded in the current master.
    # But they should be, by induction, because we extend like this.
    for train in state.builds_in_progress.values():
        if train.base == base:
            candidates = candidates - train.prs
            base = train.tip

    if len(candidates) == 0:
        return base, set()

    candidate = select(
        {pr for pr_id, pr in state.open_prs.items() if pr_id in candidates}
    )
    return base, {candidate}


def strategy_classic(state: State) -> Tuple[Commit, set[PrId]]:
    """
    Build pull requests in pull request id order. Does not do rollups. In case
    of parallel builds, this optimistically assumes all builds pass, and it
    builds upon any existing builds.
    """
    return strategy_optimistic_parallel(
        state,
        lambda candidates: min(pr.id_ for pr in candidates),
    )


def strategy_fifo(state: State) -> Tuple[Commit, set[PrId]]:
    """
    Build pull requests in approval order. Does not do rollups. In case
    of parallel builds, this optimistically assumes all builds pass, and it
    builds upon any existing builds.
    """
    return strategy_optimistic_parallel(
        state,
        lambda candidates: sorted((pr.arrived_at, pr.id_) for pr in candidates)[0][1],
    )


def strategy_lifo(state: State) -> Tuple[Commit, set[PrId]]:
    """
    Build pull requests in reverse approval order. Does not do rollups. In case
    of parallel builds, this optimistically assumes all builds pass, and it
    builds upon any existing builds.
    """
    return strategy_optimistic_parallel(
        state,
        lambda candidates: sorted((-pr.arrived_at, pr.id_) for pr in candidates)[0][1],
    )


def strategy_bayesian(state: State) -> Tuple[Commit, set[PrId]]:
    """
    Tries to maximize the expected number of PRs it will merge. Does not support
    parallelism.
    """
    if len(state.builds_in_progress) > 0:
        return state.get_tip(), set()

    # Sort first by descending is_good probability, and when two PRs have equal
    # is_good probability (because they are new, let's say) order by PR id.
    # NOTE: Here we still have a choice to make, how to order PRs that have the
    # same probability of passing. We go for the "classic" strategy of ordering
    # by id, but we could order by reverse arrival time, to get the stack-like
    # behavior.
    candidates = sorted(
        ((p, -pr_id, pr_id) for pr_id, p in state.probabilities_good().items()),
        reverse=True,
    )

    includes: set[PrId] = set()
    p_train_is_good = 1.0
    best_expected_len = 0.0

    for p_pr_is_good, _, pr_id in candidates:
        p_success = p_train_is_good * p_pr_is_good
        expected_len = (len(includes) + 1) * p_success
        if expected_len > best_expected_len:
            print(
                f" - include={pr_id} {p_success=:.3f} "
                f"{expected_len=:.3f} {p_pr_is_good=:.3f}"
            )
            includes.add(pr_id)
            best_expected_len = expected_len
            p_train_is_good = p_success
        else:
            break

    # If we build a single pull request, we are guaranteed to reduce the size of
    # the backlog by one. So if all of the trains we can build are quite bad,
    # then instead, we can confirm a likely-bad PR.
    if best_expected_len < 1.0:
        p_pr_is_good, _, pr_id = candidates[-1]
        print(
            f" - Opting to confirm the worst candidate ({pr_id}) instead, "
            f"{p_pr_is_good=:.3f}"
        )
        return state.get_tip(), {pr_id}

    return state.get_tip(), includes


def strategy_minimize_entropy_wait(state: State) -> Tuple[Commit, set[PrId]]:
    """
    Tries to minimize the total waiting time, based on two factors:

    * The expected number of pull requests merged.
    * The expected entropy of the remaining distribution.
    """
    base = state.get_tip()
    pd = state.pd

    # The size of the set of open PRs is the real number we care about, the size
    # of the probability distribution is capped. However, sometimes PRs that are
    # no longer open can still be in the distribution, so we take the max of the
    # sizes.
    n = max(len(pd.prs), len(state.open_prs))
    candidates = []

    options = list(pd.speculate_subsets())
    for s, sp in enumerate(options):
        candidates.append((sp.expected_wait_time(queue_size=n), sp, s, base))

    for train in reversed(state.builds_in_progress.values()):
        root, prs_since_root = next(iter(train.root_path.items()))
        if root == base and all(pr in state.open_prs for pr in prs_since_root):
            s_root = pd.pack(prs_since_root)
            sp_base = options[pd.pack(prs_since_root)]
            if sp_base.p_success == 0.0:
                continue

            base = train.tip
            pd = state.pd.observe_outcome(train.prs, is_good=True)
            for s, sp in enumerate(pd.speculate_subsets()):
                if s & s_root > 0:
                    # Skip options that overlap with the parent. We only want to
                    # build new stuff now.
                    continue

                len_success = (n - sp.n_confirmed_success) * sp.entropy_success
                len_failure = (n - sp.n_confirmed_failure) * sp.entropy_failure
                len_base_failure = (n - sp_base.n_confirmed_failure) * sp_base.entropy_failure
                expected_wait_time = (
                    sp_base.p_success * len_success * sp.p_success +
                    sp_base.p_success * len_failure * (1.0 - sp.p_success) +
                    (1.0 - sp_base.p_success) * len_base_failure
                )
                candidates.append((expected_wait_time, sp, s, base))

            break

    candidates.sort(
        # Order by ascending expected wait time, break ties by descending number
        # of PRs confirmed on success, then break ties by descending subset
        # bitmask, which means we prefer subsets of younger PRs, and we prefer
        # non-empty subsets over the empty subset. Edit: Or not, does this mess
        # things up?
        key=lambda tup: (tup[0], -tup[1].n_confirmed_success, tup[2]),
    )
    for expected_wait_time, sp, s, base in candidates[:5]:
        s_str = f"{s:b}".zfill(len(pd.prs))
        print(f" - Option {s_str} {expected_wait_time=:.3f} {sp} {base=}")

    _, sp, s, base = candidates[0]

    includes = {pr_id for pr_id in pd.unpack(s) if pr_id in state.open_prs}
    return base, includes


def main() -> None:
    configs = [
        #Config.new(parallelism=1, criticality=0.25),
        #Config.new(parallelism=1, criticality=0.50),
        #Config.new(parallelism=1, criticality=1.00),
        #Config.new(parallelism=2, criticality=0.25),
        #Config.new(parallelism=2, criticality=0.50),
        #Config.new(parallelism=2, criticality=1.00),
        #Config.new(parallelism=4, criticality=0.25),
        Config.new(parallelism=4, criticality=0.50),
        #Config.new(parallelism=4, criticality=1.00),
    ]
    strategies = [
        ("minimize_entropy_wait", strategy_minimize_entropy_wait),
        # ("bayesian", strategy_bayesian),
        # ("classic", strategy_classic),
        # ("fifo", strategy_fifo),
        # ("lifo", strategy_lifo),
    ]
    for config in configs:
        for strategy_name, strategy in strategies:
            runs = []
            for seed in range(250):
                sim = Simulator.new(
                    seed=seed,
                    config=config,
                    strategy=strategy,
                )
                sim.run_to_last_pr()
                runs.append(sim)

            plot_results(config, strategy_name, runs)


if __name__ == "__main__":
    main()
