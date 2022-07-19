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
    prior_is_good_probability: float = 0.8
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

    def observe_outcome(self, ids: set[PrId], is_good: bool) -> ProbDist:
        """
        Perform a Bayesian update, for the evidence that the set of pull
        requests built together was good/bad.
        """
        mask = sum(1 << self.prs.index(pr) for pr in ids)
        n = len(self.ps)
        ps: list[float] = []

        if is_good:
            p_outcome = sum(self.ps[i] for i in range(n) if i & mask == mask)
        else:
            p_outcome = sum(self.ps[i] for i in range(n) if i & mask != mask)

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
        while len(ps) > 4:
            m = len(ps) // 2
            ps = [ps[i] + "  " + ps[m + i] for i in range(m)]

        result.extend("  " + line for line in ps)
        result.append("])")
        return "\n".join(result)

    def entropy(self) -> float:
        """Return the Shannon entropy of the distribution."""
        # Throw in a max to avoid negative zero, because it looks ugly.
        return max(0.0, -sum(p * math.log2(p) for p in self.ps if p > 0.0))

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

        prs_good = set()
        prs_bad = set()

        for i, pr in enumerate(self.prs):
            index_mask = 1 << i
            if mask_good & index_mask > 0:
                prs_good.add(pr)
            if mask_bad & index_mask > 0:
                prs_bad.add(pr)

        return prs_good, prs_bad


p = ProbDist.new().insert(PrId(1), 0.9).insert(PrId(2), 0.9).insert(PrId(3), 0.9).insert(PrId(4), 0.9)
print(p, p.prs_confirmed())

p = p.observe_outcome({PrId(1)}, is_good=True)
print(p, p.prs_confirmed())

p = p.observe_outcome({PrId(2), PrId(3)}, is_good=False)
print(p, p.prs_confirmed())

p = p.observe_outcome({PrId(3)}, is_good=False)
print(p, p.prs_confirmed())

p = p.remove(PrId(1)).remove(PrId(4))
print(p)

import sys
sys.exit(1)


class State(NamedTuple):
    open_prs: dict[PrId, PullRequest]

    # Per merged or failed PR, the time from arrival until it got merged.
    closed_prs: dict[PrId, Time]

    # Pull requests that have been confirmed to be good. Note that that does not
    # necessarily mean that they got merged, they might have been built upon the
    # wrong head.
    good_prs: set[PrId]

    # For pull requests that we have built before, but which we haven't
    # confirmed to be good, the estimated probability that they are good based
    # on the current evidence.
    probabilities_good: dict[PrId, float]

    # Builds that failed, and the pull requests that they contained. After a
    # pull request was built successfully, we removed it from all sets here, so
    # every individual set shrinks over time. This also allows us to confirm
    # pull requests as bad without building them in isolation by confirming that
    # the complement is good.
    builds_failed: list[set[PrId]]

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
            good_prs=set(),
            probabilities_good={},
            builds_failed=[],
            builds_in_progress={},
            heads=[Commit(0)],
            num_build_slots=num_build_slots,
            prior_is_good_probability=prior_is_good_probability,
        )

    def get_tip(self) -> Commit:
        """Return the current tip of the master branch."""
        return self.heads[-1]

    def probability_good(self, pr: PrId) -> float:
        """Return the probability that a given pull request is good."""
        if pr in self.good_prs:
            return 1.0

        if pr in self.closed_prs:
            # If a pull request is closed but not good, it must be bad.
            return 0.0

        return self.probabilities_good.get(pr, self.prior_is_good_probability)

    def probability_all_good(self, prs: Iterable[PrId]) -> float:
        """Return the probability that all PRs in the sequence are good."""
        p: float = np.product([self.probability_good(y) for y in prs])
        return p

    def _recompute_probabilities_good(self, print_update: bool) -> None:
        """
        Recompute the is-good probabilities based on the set of good prs and the
        list of failed builds. Note, this method mutates the instance! It should
        be called after creating a new instance with _replace that changes the
        failed builds or good prs.
        """
        ps_old = self.probabilities_good.copy()

        # Start again from a clean slate without any evidence (aside from that
        # for the pull requests that succeeded), and then apply all evidence at
        # once from that state using Bayes' rule. This is only one possible way
        # of doing this, we could also apply e.g. every failed build one by one,
        # and we would get a different outcome. One thing I like about this
        # formulation below, is that it does not depend on the order in which
        # the evidence came in, while the other one does. A downside is that it
        # leads to less extreme estimates about pull requests. I asked about
        # this on Stack Exchange: https://stats.stackexchange.com/q/582275/140869.
        self.probabilities_good.clear()
        bad_prs = {x for fails in self.builds_failed for x in fails}
        trains_per_pr = {
            x: [fails for fails in self.builds_failed if x in fails] for x in bad_prs
        }
        updates = {}
        for x, trains in trains_per_pr.items():
            # Perform the Bayesian update for the is-good probability of this
            # pull request, given that it was involved in all those failing
            # builds.
            p_train_fails = [1.0 - self.probability_all_good(train) for train in trains]
            p_all_trains_fail = np.product(p_train_fails)

            p_train_fails_given_x_is_good = [
                1.0 - self.probability_all_good(y for y in train if y != x)
                for train in trains
            ]
            p_all_trains_fail_given_x_is_good = np.product(
                p_train_fails_given_x_is_good
            )
            p_x_is_good = self.probability_good(x)
            p_x_is_good_given_all_trains_failed = (
                p_all_trains_fail_given_x_is_good * p_x_is_good / p_all_trains_fail
            )
            updates[x] = p_x_is_good_given_all_trains_failed

        self.probabilities_good.update(updates)

        assert all(0.0 <= p <= 1.0 for p in self.probabilities_good.values())

        if print_update:
            for x, p_old in ps_old.items():
                p_new = self.probability_good(x)
                if p_new != p_old and p_new != 1.0:
                    print(f"is_good({x}): {p_old:.3f} -> {p_new:.3f}")

    def insert_pr(self, pr: PullRequest) -> State:
        return self._replace(open_prs=self.open_prs | {pr.id_: pr})

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
        new_good_prs = self.good_prs | good_prs
        new_builds_failed = [prs - good_prs for prs in self.builds_failed]

        new_open_prs = dict(self.open_prs.items())
        new_closed_prs = dict(self.closed_prs.items())

        def close_pr(pr_id: PrId) -> None:
            pr = new_open_prs[pr_id]
            del new_open_prs[pr_id]
            dt = Time(t - pr.arrived_at)
            assert dt > Time(0.0)
            assert pr_id not in new_closed_prs
            new_closed_prs[pr_id] = dt

        # When we learn that some pull requests are good, we might also learn
        # that some other pull requests are bad: when they were part of a
        # failing build and all pull requests except for a single one were good,
        # the remaining one must be bad.
        for failed_prs in new_builds_failed:
            if len(failed_prs) == 1:
                pr_id = next(iter(failed_prs))
                if pr_id in new_open_prs:
                    print(f"Confirmed bad indirectly: {pr_id}")
                    close_pr(pr_id)

        tip = self.get_tip()
        if tip not in build.root_path:
            # This success was not on top of master, so it was not that useful,
            # though the new probabilities can still help to get these prs
            # prioritized next.
            result = self._replace(
                open_prs=new_open_prs,
                closed_prs=new_closed_prs,
                good_prs=new_good_prs,
                builds_in_progress=new_builds,
                builds_failed=new_builds_failed,
            )
            result._recompute_probabilities_good(print_update=True)
            return result

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

        result = self._replace(
            open_prs=new_open_prs,
            closed_prs=new_closed_prs,
            good_prs=new_good_prs,
            builds_in_progress=new_builds,
            builds_failed=new_builds_failed,
            heads=new_heads,
        )
        result._recompute_probabilities_good(print_update=True)
        return result

    def assume_failed(self, t: Time, bad_prs: set[PrId], print_update: bool) -> State:
        """
        Hypothetically assume that a build with the given pull requests in it
        failed. Note, to finish a build, use `complete_build_failure` below.
        """
        new_open_prs = self.open_prs
        new_closed_prs = self.closed_prs

        # If the build contained a single PR, then we mark that PR as failed.
        if len(bad_prs) == 1:
            pr_id = next(iter(bad_prs))

            # We might have a stale build result; possibly a different build
            # already concluded that this PR failed to build. Then there is
            # nothing to update.
            if pr_id in self.open_prs:
                assert pr_id not in self.closed_prs
                assert pr_id not in self.good_prs

                pr = self.open_prs[pr_id]
                dt = Time(t - pr.arrived_at)
                assert dt > Time(0.0)
                assert pr_id not in self.closed_prs
                new_open_prs = {k: v for k, v in self.open_prs.items() if k != pr_id}
                new_closed_prs = self.closed_prs | {pr_id: dt}

            # No need to record the failure if it was a single PR.
            new_builds_failed = self.builds_failed

        elif len(bad_prs) > 1:
            new_builds_failed = self.builds_failed + [bad_prs]

        else:
            raise Exception("A build failed, but it contained zero bad PRs?")

        result = self._replace(
            open_prs=new_open_prs,
            closed_prs=new_closed_prs,
            builds_failed=new_builds_failed,
        )
        result._recompute_probabilities_good(print_update=print_update)
        return result

    def complete_build_failure(self, t: Time, id_: BuildId) -> State:
        """
        Complete the build, update the is-good probabilities for the PRs
        involved, or if the train was a singleton, mark that PR as failed.
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

        return self.assume_failed(t, bad_prs, print_update=True)._replace(builds_in_progress=new_builds)


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
        (
            (state.probability_good(pr_id), -pr_id, pr_id)
            for pr_id in state.open_prs.keys()
        ),
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


def iterate_options(
    state: State,
    excludes: set[PrId],
) -> Iterable[Tuple[set[PrId], str]]:
    """
    Propose some things that we could build, starting from the given state.
    In addition to the pull requests to build, also return the reason, just for
    diagnostic purposes.
    """
    # Sort first by descending is-good probability, and when two PRs have equal
    # is-good probability (because they are new, let's say) order by PR id.
    # Some alternatives to try could be to order by (reverse) arrival time,
    # although it matters less for the Bayesian strategy than for the others,
    # because it builds many more PRs, so few PRs stay unevaluated.
    candidates = set(state.open_prs.keys()) - excludes

    if len(candidates) == 0:
        return

    candidates_ranked = sorted(
        ((state.probability_good(pr_id), -pr_id, pr_id) for pr_id in candidates),
        reverse=True,
    )

    # The most straightforward option: include the pull requests that are most
    # likely to succeed in the next build. Due to the eager way we explore
    # combinations later on, we have to provide the full set as part of these
    # options, if we only try a single pull request, we might not explore the
    # path further even though it was optimal.
    p = 1.0
    best_len = 0.0
    for i in range(0, len(candidates_ranked)):
        p_good, _, pr_id = candidates_ranked[i]
        p_next = p * p_good
        expected_len = (i + 1) * p_next

        if expected_len < best_len:
            # Continue adding PRs, from most-likely to succeed to less likely,
            # until the expected length no longer grows. Growing the set is a
            # trade-off between increasing progress in case of success, but
            # decreasing the success probability.
            break

        yield {pr_id for _, _, pr_id in candidates_ranked[:i + 1]}, (
            f"Set of highest success probability, p_good={p_next:.3f} "
            f"n={i + 1} {expected_len=:.3f}"
        )

        p = p_next
        best_len = expected_len

    # Alternatively, we could try to confirm a likely-bad pull request as bad
    # by building it in isolation.
    p, _, worst_pr_id = candidates_ranked[-1]
    yield {worst_pr_id}, f"Highest fail probability, p_good={p:.3f}"

    # Yet another option is to build the complement of one of the failed builds.
    # That might allow us to confirm one PR as bad and at the same time get some
    # others merged.
    for failed_prs in state.builds_failed:
        prs = sorted(failed_prs, key=lambda pr_id: state.probability_good(pr_id))
        p = state.probability_good(prs[0])
        to_build = {pr for pr in prs if pr in state.open_prs and pr not in excludes}
        if len(to_build) == 0:
            continue
        p_success = state.probability_all_good(to_build)
        expected_len = (len(to_build) + 1) * p_success
        yield to_build, (
            f"Complement of failed build, worst p_good={p:.3f} "
            f"{p_success=:.3f} {expected_len=:.3f}"
        )


def expected_num_processed(
    state: State,
    root_path: list[set[PrId]],
    print_explain_indent: Optional[str] = None,
) -> float:
    """
    Return the expected number of pull requests that the backlog will have
    shrunken after we learn the outcome of this build. (Not necessarily due to
    this build itself, also due to builds that this one is based on.)

    This takes the full root path, where every subsequent element must be a
    subset of the previous one. The final element contains all *new* pull
    requests that we did not include before.
    """
    if len(root_path) == 0:
        return 0.0

    if len(state.open_prs) == 0:
        # We can't make progress if there are no open PRs.
        return 0.0

    expected_len = 0.0
    prs = root_path[0]
    new_prs = root_path[-1]
    base = prs - new_prs

    assert len(prs) >= len(new_prs)
    if len(prs) == 0:
        # We can't make progress if we did not build anything.
        return 0.0

    # Case 1: the build passes.
    p_success = state.probability_all_good(prs)
    n_merged = len(prs)

    prs_failed: set[PrId] = set()
    n_failed = 0
    for failed_prs in state.builds_failed:
        leftover = {
            pr for pr in failed_prs
            if pr not in prs and pr in state.open_prs
        }
        if len(leftover) == 1:
            prs_failed = prs_failed | leftover
    n_failed = len(prs_failed)

    expected_len += p_success * (n_merged + n_failed)
    if print_explain_indent is not None:
        print(
            f"{print_explain_indent}Expected len from success: "
            f"{n_merged=} {n_failed=} {p_success=:.3f} >> {expected_len=:.3f}"
        )

    # Case 2: the build fails. Any further things we investigate and query
    # the state for, are *given* that the build failed, so we should update
    # the state to compute conditional probabilities.
    p_failure = 1.0 - p_success

    bad_prs = prs - state.good_prs
    if len(bad_prs) > 0:
        # We need to pass a time to advance the state.
        dummy_time = Time(1.0 + max(pr.arrived_at for pr in state.open_prs.values()))
        # TODO: Is this the culprit?
        #state = state.assume_failed(dummy_time, bad_prs, print_update=False)

    # We might learn something directly. If we built a single pull request
    # on top of the base. We learn that only when the base passed and this
    # build failed.
    if len(new_prs) == 1:
        p_base_success = state.probability_all_good(base)
        expected_len += p_failure * p_base_success
        if print_explain_indent is not None:
            print(
                f"{print_explain_indent}Expected len from direct failure: "
                f"{p_failure=:.3f} {p_base_success=:.3f} >> {expected_len=:.3f}"
            )

    # When our build fails, even if we did not learn anything from our
    # build, the one that we based it on might still have succeeded. But the
    # probability that it did, not that we know ours failed, should be
    # slightly lower than without that information.
    base_expected_len = expected_num_processed(
        state,
        root_path=[prs - new_prs for prs in root_path[:-1]],
        print_explain_indent=None if print_explain_indent is None else print_explain_indent + "  "
    )
    expected_len += p_failure * base_expected_len
    if print_explain_indent is not None:
        print(
            f"{print_explain_indent}Expected len from parent: "
            f"{p_failure=:.3f} {base_expected_len=:.3f} >> {expected_len=:.3f}"
        )

    return expected_len


def maximize_num_processed(
    state: State,
    root_path: list[set[PrId]],
) -> Tuple[float, set[PrId], list[str]]:
    """
    Given the root path of existing builds to build on top of, try various
    things that we can build on top of it, and return the option that maximizes
    the expected number of pull requests processed (merged or failed) after that
    build completes. The prs in `include` are always part of the returned
    solution. Returns (expected_len, prs_to_build, reasons_for_selecting).
    """
    base_expected_len = expected_num_processed(state, root_path)
    best_option: Tuple[float, set[PrId], list[str]] = (base_expected_len, set(), [])
    made_progress = True

    already_being_built = set() if len(root_path) == 0 else root_path[0]

    while made_progress:
        made_progress = False
        _, include, base_reasons = best_option
        for to_add, reason in iterate_options(state, excludes=include | already_being_built):
            to_build = include | to_add
            new_root_path = [prs | to_build for prs in root_path] + [to_build]
            expected_len = expected_num_processed(state, new_root_path)

            if expected_len > best_option[0]:
                best_option = (expected_len, to_build, base_reasons + [reason])
                made_progress = True

    return best_option


def strategy_bayesian_mkiv(state: State) -> Tuple[Commit, set[PrId]]:
    """
    Tries to minimize the remaining total waiting time.
    """
    best_option = maximize_num_processed(state, root_path=[])
    base = state.get_tip()
    best_root_path = []

    # If there are builds in progress, we can also try building on top of them.
    for build_id, build in state.builds_in_progress.items():
        if build.get_root() != state.get_tip():
            # We only want to build on top of builds that build on master, not
            # on builds that are no longer relevant.
            continue

        base_prs = build.prs_since_root()

        if any(pr not in state.open_prs for pr in base_prs):
            # We also don't want to build on top of builds that are guaranteed
            # to fail.
            continue

        root_path = list(build.root_path.values())
        option = maximize_num_processed(state, root_path)
        if option[0] > best_option[0]:
            print(
                f" - Replacing option expected_len={best_option[0]:.3f} "
                f"with expected_len={option[0]:.3f} on top of "
                f"build {build_id=} {build.tip=}"
            )
            best_option = option
            base = build.tip
            best_root_path = root_path

    print(f"Selected prs={best_option[1]} expected_len={best_option[0]:.3f} because:")
    for reason in best_option[2]:
        print(f" - {reason}")

    prs = best_option[1]
    expected_len_base = expected_num_processed(state, best_root_path)
    best_root_path = [x | prs for x in best_root_path] + [prs]
    expected_len_new = expected_num_processed(state, best_root_path, print_explain_indent="")
    print(f"Recomputed {expected_len_new=:.3f} {expected_len_base=:.3f}")

    return base, best_option[1]

    #prs_not_being_built_exclusively = set(state.open_prs.keys())
    #for build in state.builds_in_progress.values():
    #    if len(build.prs) == 1:
    #        prs_not_being_built_exclusively = (
    #            prs_not_being_built_exclusively - build.prs
    #        )


def main() -> None:
    configs = [
        # Config.new(parallelism=1, criticality=0.25),
        # Config.new(parallelism=1, criticality=0.50),
        # Config.new(parallelism=1, criticality=1.00),
        # Config.new(parallelism=2, criticality=0.25),
        # Config.new(parallelism=2, criticality=0.50),
        # Config.new(parallelism=2, criticality=1.00),
        # Config.new(parallelism=4, criticality=0.25),
        # Config.new(parallelism=4, criticality=0.50),
        Config.new(parallelism=4, criticality=1.00),
    ]
    strategies = [
        ("bayesian_mkiv", strategy_bayesian_mkiv),
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
