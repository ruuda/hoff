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
    prior_is_good_probability: float = 0.7
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


class State(NamedTuple):
    open_prs: dict[PrId, PullRequest]
    # Per merged or failed PR, the time from arrival until it got merged.
    closed_prs: dict[PrId, Time]
    is_good_probabilities: dict[PrId, float]
    builds_in_progress: dict[BuildId, Build]

    # The successive commits that the master branch pointed to.
    heads: list[Commit]

    @staticmethod
    def new(prior_is_good_probability: float) -> State:
        return State(
            open_prs={},
            closed_prs={},
            is_good_probabilities=defaultdict(lambda: prior_is_good_probability),
            builds_in_progress={},
            heads=[Commit(0)],
        )

    def get_tip(self) -> Commit:
        """Return the current tip of the master branch."""
        return self.heads[-1]

    def probability_all_good(self, prs: Iterable[PrId]) -> float:
        """Return the probability that all PRs in the sequence are good."""
        p: float = np.product([self.is_good_probabilities[y] for y in prs])
        return p

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

        new_ps = self.is_good_probabilities.copy()
        for pr in build.prs_since_root():
            # We now know that this pull request was good. Don't set it quite to
            # 1.0 to avoid division by zero problems.
            new_ps[pr] = 0.9999

        tip = self.get_tip()
        if tip not in build.root_path:
            # This success was not on top of master, so it was not that useful,
            # though the new probabilities can still help to get these prs
            # prioritized next.
            return self._replace(
                builds_in_progress=new_builds,
                is_good_probabilities=new_ps,
            )

        # If we merge the tip of this build, then that means that some other
        # builds that were previously speculative, might now be directly on top
        # of master, so update their root paths.
        new_builds = {
            bid: b.trim_root_path(new_tip=build.tip) for bid, b in new_builds.items()
        }

        prs_since_tip = build.root_path[tip]

        new_open_prs = dict(self.open_prs.items())
        new_closed_prs = dict(self.closed_prs.items())
        new_heads = self.heads + [build.tip]

        for pr_id in prs_since_tip:
            pr = new_open_prs[pr_id]
            del new_open_prs[pr_id]
            dt = Time(t - pr.arrived_at)
            assert dt > Time(0.0)
            assert pr_id not in new_closed_prs
            new_closed_prs[pr_id] = dt

        return self._replace(
            open_prs=new_open_prs,
            closed_prs=new_closed_prs,
            builds_in_progress=new_builds,
            heads=new_heads,
            is_good_probabilities=new_ps,
        )

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

        new_ps = self.is_good_probabilities.copy()

        # If the build contained a single PR, then instead of updating the
        # probabilities, we mark that PR as failed.
        if len(bad_prs) == 1:
            pr_id = next(iter(bad_prs))

            if pr_id not in self.open_prs:
                # We might have a stale build result; possibly a different build
                # already concluded that this PR failed to build. Then there is
                # nothing to change.
                assert pr_id in self.closed_prs
                return self._replace(builds_in_progress=new_builds)

            pr = self.open_prs[pr_id]
            dt = Time(t - pr.arrived_at)
            assert dt > Time(0.0)
            assert pr_id not in self.closed_prs
            new_open_prs = {k: v for k, v in self.open_prs.items() if k != pr_id}
            new_closed_prs = self.closed_prs | {pr_id: dt}
            # Also still update the probability to virtually zero, so that if
            # other trains involving this PR are still in progress, their
            # failure will be blamed on this PR. Don't quite set it to zero to
            # avoid division by zero problems, but also, to still cause a slight
            # update for other trains, in case they contain multiple bad PRs.
            new_ps[pr_id] = 0.01
            return self._replace(
                builds_in_progress=new_builds,
                open_prs=new_open_prs,
                closed_prs=new_closed_prs,
                is_good_probabilities=new_ps,
            )

        # Perform the Bayesian update for the is-good probabilities of the PRs
        # involved in this failed build.

        p_train_fails = 1.0 - self.probability_all_good(bad_prs)
        for x in bad_prs:
            p_train_fails_given_x_is_good = 1.0 - self.probability_all_good(
                y for y in bad_prs if y != x
            )
            p_x_is_good = self.is_good_probabilities[x]
            p_x_is_good_given_train_failed = (
                p_train_fails_given_x_is_good * p_x_is_good / p_train_fails
            )
            print(
                f"is_good({x}): {p_x_is_good:.3f} -> {p_x_is_good_given_train_failed:.3f}"
            )
            new_ps[x] = p_x_is_good_given_train_failed

        assert all(0.0 < p < 1.0 for p in new_ps.values())

        return self._replace(
            builds_in_progress=new_builds,
            is_good_probabilities=new_ps,
        )


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
            state=State.new(config.prior_is_good_probability),
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

        assert next(iter(root_path.keys())) == self.state.get_tip(), (
            "Build must directly or indirectly build upon the current tip."
        )
        assert all(pr in self.state.open_prs for pr in next(iter(root_path.values()))), (
            "Build must build upon a train which is not guaranteed to fail."
        )

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

    fig, axes = plt.subplots(nrows=1, ncols=2, tight_layout=True, figsize=(15, 5))

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
    p25, p50, p75 = np.quantile(backlog_sizes, (0.1, 0.5, 0.9), axis=0)

    # Tweak the ticks a bit for some regions, NumPy's default have too little
    # detail to make the grid lines useful, in my opinion.
    if p75[-1] > 2.5:
        if p75[-1] < 8.0:
            ax.yaxis.set_major_locator(MultipleLocator(2.0))
            ax.yaxis.set_minor_locator(MultipleLocator(0.5))
        elif p75[-1] < 21.0:
            ax.yaxis.set_major_locator(MultipleLocator(5.0))
            ax.yaxis.set_minor_locator(MultipleLocator(1.0))
        elif p75[-1] < 31.0:
            ax.yaxis.set_major_locator(MultipleLocator(5.0))
            ax.yaxis.set_minor_locator(MultipleLocator(1.0))
        elif p75[-1] < 55.0:
            ax.yaxis.set_major_locator(MultipleLocator(10.0))
            ax.yaxis.set_minor_locator(MultipleLocator(2.0))
        elif p75[-1] < 130.0:
            ax.yaxis.set_major_locator(MultipleLocator(20.0))
            ax.yaxis.set_minor_locator(MultipleLocator(10.0))

    ax.grid(color="black", linestyle="dashed", axis="y", alpha=0.1, which="both")
    ax.fill_between(
        size_sample_times,
        p25,
        p75,
        alpha=0.2,
        color="black",
        label="backlog size, p25–p75",
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

    total_prs = sum(run.config.num_prs for run in runs)
    prs_pending = total_prs - len(wait_times)
    if prs_pending / total_prs > 0.20:
        ax.text(
            0.5,
            0.5,
            "Be careful when interpreting this histogram, it only shows\n"
            "wait times for pull requests that failed or got merged, but\n"
            f"{prs_pending/total_prs:.1%} of pull requests never got to that "
            "point.",
            transform=ax.transAxes,
            horizontalalignment="center",
            bbox={
                "boxstyle": "round",
                "facecolor": "white",
                "alpha": 0.8,
            },
        )

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
            (state.is_good_probabilities[pr_id], -pr_id, pr_id)
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


def expected_num_processed(state: State, prs: set[PrId]) -> float:
    """
    Return the expected number of pull requests that the backlog will shrink
    after we learn the outcome of building `prs` all together.
    """
    if len(prs) == 1:
        # For a single PR, the backlog shrinks by 1 with certainty. Either it
        # succeeds and we merge it, or the build fails and we fail the PR.
        return 1.0

    # Otherwise, we merge all of them with P(all good), otherwise we merge zero.
    return state.probability_all_good(prs) * len(prs)


def maximize_num_processed(
    state: State,
    includes: set[PrId],
    candidates: Iterable[PrId],
) -> Tuple[set[PrId], float, float]:
    """
    Return the set of pull requests to build, which is a superset of `includes`,
    and which maximizes the expected number of pull requests processed. Also
    return the probability that this set will build successfully, and the
    expected number of pull requests merged after the build completes (assuming
    that `includes` are already being built).
    """
    # Sort first by descending is_good probability, and when two PRs have equal
    # is_good probability (because they are new, let's say) order by PR id.
    # NOTE: Here we still have a choice to make, how to order PRs that have the
    # same probability of passing. We go for the "classic" strategy of ordering
    # by id, but we could order by reverse arrival time, to get the stack-like
    # behavior.
    candidates_ranked = sorted(
        ((state.is_good_probabilities[pr_id], -pr_id, pr_id) for pr_id in candidates),
        reverse=True,
    )
    prs = includes
    p_all_good = state.probability_all_good(prs)
    expected_len = p_all_good * len(prs)
    expected_len_includes = expected_len

    for p_is_good, _, pr in candidates_ranked:
        if pr in prs:
            continue

        new_prs = prs | {pr}
        new_p_all_good = p_all_good * p_is_good

        # The new expected amount of PRs processed can be broken down in two
        # cases. If the train succeeds, then processed len(prs). But if the
        # build fails, we don't process zero: the base train with the `includes`
        # PRs can still succeed, so we should count its expected number of PRs
        # processed too.
        new_expected_len = (
            new_p_all_good * len(new_prs)
            + (1.0 - new_p_all_good) * expected_len_includes
        )

        if new_expected_len < expected_len:
            return prs, p_all_good, expected_len

        prs = new_prs
        p_all_good = new_p_all_good
        expected_len = new_expected_len

    return prs, p_all_good, expected_len


def strategy_bayesian_mkii(state: State) -> Tuple[Commit, set[PrId]]:
    """
    Tries to minimize the remaining total waiting time.
    """
    best_alternative_expected_len = 0.0

    prs_not_being_built_exclusively = set(state.open_prs.keys())
    for build in state.builds_in_progress.values():
        if len(build.prs) == 1:
            prs_not_being_built_exclusively = prs_not_being_built_exclusively - build.prs

    if len(prs_not_being_built_exclusively) == 0:
        # If everything is already building, we have no new builds to start.
        # TODO: Potentially we could make one of the existing builds more fine-
        # grained though.
        return state.get_tip(), set()

    # We can always try to build on top of the current tip.
    base = state.get_tip()
    prs, p_all_good, expected_len = maximize_num_processed(
        state,
        includes=set(),
        candidates=prs_not_being_built_exclusively,
    )
    print(f" - Main candidate: {expected_len=:.3f} {p_all_good=:.3f} {base=} {prs=}")

    # If there are builds in progress, we can also try building on top of them.
    for build in state.builds_in_progress.values():
        if build.get_root() != state.get_tip():
            # We only want to build on top of builds that build on master, not
            # on builds that are no longer relevant.
            continue

        base_prs = build.prs_since_root()

        if any(pr not in state.open_prs for pr in base_prs):
            # We also don't want to build on top of builds that are guaranteed
            # to fail.
            continue

        best_alternative_expected_len = max(
            best_alternative_expected_len,
            expected_num_processed(state, base_prs),
        )

        new_prs, p_all_good, new_expected_len = maximize_num_processed(
            state,
            includes=base_prs,
            # We consider all PRs again here, not only the ones that are not
            # being built already, because we are extending a chain, so we
            # believe that chain would succeed, and any other competing builds
            # would not get merged.
            candidates=state.open_prs.keys(),
        )

        if new_expected_len > expected_len:
            prs = new_prs - base_prs
            expected_len = new_expected_len
            base = build.tip
            print(
                f" - Better candidate: {expected_len=:.3f} {p_all_good=:.3f} {base=} {prs=}"
            )

    # TODO: Implement the "split if build slots left"-trick.

    # As an alternative to trying to continue extending master, we can try to
    # confirm a likely-bad pull request.
    p_is_good, worst_pr = min(
        (state.is_good_probabilities[pr_id], pr_id) for pr_id in prs_not_being_built_exclusively
    )
    p_fail_confirmed = 1.0 - p_is_good
    new_expected_len = best_alternative_expected_len + p_fail_confirmed
    if new_expected_len > expected_len:
        base = state.get_tip()
        prs = {worst_pr}
        print(
            f" - Opting to confirm bad pull request {worst_pr}: "
            f"expected_len={new_expected_len:.3f} {p_fail_confirmed=:.3f}"
        )

    return base, prs


def strategy_bayesian_parallel(state: State) -> Tuple[Commit, set[PrId]]:
    """
    Tries to maximize the expected number of PRs it will merge.
    """
    if len(state.builds_in_progress) == 0:
        return strategy_bayesian(state)

    dummy_time = Time(max(pr.arrived_at + 10.0 for pr in state.open_prs.values()))

    best_len = 0.0
    best_prs = set()
    best_base = state.get_tip()

    for bid, build in state.builds_in_progress.items():
        print(f" > Bayesian parallel, base={build}")

        if build.get_root() != state.get_tip():
            # If this in-progress build is no longer on top of master (because
            # something conflicting was merged since that build was started)
            # then we should not build on top of it.
            continue

        prs_in_build = build.prs_since_root()
        if not all(p in state.open_prs for p in prs_in_build):
            # It could also be that the in-progress build is on top of master,
            # but it includes PRs that have failed in the meantime. In that case
            # we can no longer investigate the hypothetical case where it
            # succeeds, so for simplicity we skip this build entirely.
            continue

        p_build_succeeds = state.probability_all_good(prs_in_build)
        state_success = state.complete_build_success(dummy_time, bid)._replace(
            builds_in_progress={}
        )
        state_failure = state.complete_build_failure(dummy_time, bid)._replace(
            builds_in_progress={}
        )

        if len(state_success.open_prs) > 0:
            base_succ, prs_succ = strategy_bayesian(state_success)
            # NB: The expected number of PRs we merge here does *not* include
            # the PRs from the parent build that we assume succeeded here. We
            # are interested in how much *this* additional build that we are
            # constructing would shrink the backlog, so contributions from the
            # parent do not count.
            len_succ = p_build_succeeds * expected_num_processed(
                state_success, prs_succ
            )
            print(
                f" > Success: expected_len={len_succ:.3f} "
                f"p={p_build_succeeds:.3f} base={base_succ} prs={prs_succ}"
            )

            if len_succ > best_len:
                best_len = len_succ
                best_prs = prs_succ
                best_base = base_succ

        if len(state_failure.open_prs) > 0:
            base_fail, prs_fail = strategy_bayesian(state_failure)
            len_fail = (1.0 - p_build_succeeds) * expected_num_processed(
                state_failure, prs_fail
            )
            print(
                f" > Failure: expected_len={len_fail:.3f} "
                f"p={1.0 - p_build_succeeds:.3f} "
                f"base={base_fail} prs={prs_fail}"
            )

            if len_fail > best_len:
                best_len = len_fail
                best_prs = prs_fail
                best_base = base_fail

    return best_base, best_prs


def main() -> None:
    configs = [
        Config.new(parallelism=1, criticality=0.15),
        Config.new(parallelism=1, criticality=0.80),
        Config.new(parallelism=1, criticality=1.00),
        Config.new(parallelism=1, criticality=1.10),
        Config.new(parallelism=1, criticality=2.0),
        Config.new(parallelism=2, criticality=0.15),
        Config.new(parallelism=2, criticality=0.80),
        Config.new(parallelism=2, criticality=1.05),
        Config.new(parallelism=4, criticality=0.15),
        Config.new(parallelism=4, criticality=0.60),
        Config.new(parallelism=4, criticality=1.01),
    ]
    strategies = [
        ("bayesian_mkii_0.7_prior", strategy_bayesian_mkii),
        # ("bayesian", strategy_bayesian),
        # ("bayesian_parallel", strategy_bayesian_parallel),
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
