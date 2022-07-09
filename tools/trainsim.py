#!/usr/bin/env python3

"""
For testing https://github.com/channable/hoff/issues/77#issuecomment-1179430191
"""

from __future__ import annotations

import math
import matplotlib  # type: ignore
import numpy as np
import heapq

from dataclasses import dataclass
from matplotlib import pyplot as plt
from matplotlib.font_manager import FontProperties  # type: ignore
from numpy.random import Generator
from numpy.typing import ArrayLike
from typing import Callable, NamedTuple, NewType, Tuple

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
    name: str
    avg_time_between_prs: Time
    avg_time_to_approve: Time = Time(60.0)
    avg_build_time: Time = Time(10.0)
    build_time_stdev: Time = Time(1.0)
    probability_pr_is_good: float = 0.9
    num_prs: int = 250
    num_build_slots: int = 2

    @staticmethod
    def subcritical() -> Config:
        # In this case, the average time between PRs is greater than the build
        # time, so the system should be able to keep up, and have an empty queue
        # much of the time. There might be an occasional backlog, but we can
        # process it quickly.
        return Config(name="subcritical", avg_time_between_prs=Time(15.0))

    @staticmethod
    def critical() -> Config:
        # In this case, the average time between PRs is equal to the build time.
        # This means that rate of the "producer" and "consumer" are matched, so
        # on average we can keep up. But in this regime, when a backlog is there
        # because we were unlucky for some time, on average we don't clear it.
        # And when we are lucky for a span of time, the backlog size can't fall
        # below zero. So over time, the backlog still grows!
        return Config(name="critical", avg_time_between_prs=Time(10.0))

    @staticmethod
    def supercritical() -> Config:
        # In this case, the average time between PRs is smaller than the build
        # time, so any strategy without parallelism or rollups will not be able
        # to cope and cause an ever-growing backlog.
        return Config(name="supercritical", avg_time_between_prs=Time(4.5))


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


class Train(NamedTuple):
    # Commit that this train was built on top of.
    base: Commit

    # Last commit in this train, which would become the new tip of the master
    # branch if we merged this train.
    tip: Commit

    # Ids of the included pull requests.
    prs: set[PrId]


class State(NamedTuple):
    open_prs: dict[PrId, PullRequest]
    # Per merged or failed PR, the time from arrival until it got merged.
    closed_prs: dict[PrId, Time]
    is_good_probabilities: dict[PrId, float]
    builds_in_progress: dict[BuildId, Train]
    master_tip: Commit

    @staticmethod
    def new() -> State:
        return State(
            open_prs={},
            closed_prs={},
            is_good_probabilities={},
            builds_in_progress={},
            master_tip=Commit(0),
        )

    def insert_pr(self, pr: PullRequest) -> State:
        return self._replace(open_prs=self.open_prs | {pr.id_: pr})

    def start_build(self, id_: BuildId, train: Train) -> State:
        return self._replace(builds_in_progress=self.builds_in_progress | {id_: train})

    def complete_build_success(self, t: Time, id_: BuildId) -> State:
        """
        Complete the build, advance the tip of the master branch if applicable,
        remove any merged PRs from the state, and record the time elapsed since
        the PR arrived for all merged PRs, in addition to the new state.
        """
        train = self.builds_in_progress[id_]
        new_builds = {
            bid: b for bid, b in self.builds_in_progress.items() if bid != id_
        }

        if train.base != self.master_tip:
            # This success was not useful, ignore it.
            # TODO: We could still update the probabilities though.
            return self._replace(builds_in_progress=new_builds)

        new_open_prs = dict(self.open_prs.items())
        new_closed_prs = dict(self.closed_prs.items())

        for pr_id in train.prs:
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
            master_tip=train.tip,
        )

    def complete_build_failure(self, t: Time, id_: BuildId) -> State:
        """
        Complete the build, update the is-good probabilities for the PRs
        involved, or if the train was a singleton, mark that PR as failed.
        """
        train = self.builds_in_progress[id_]
        new_builds = {
            bid: b for bid, b in self.builds_in_progress.items() if bid != id_
        }

        # If the train contained a single PR, then instead of updating the
        # probabilities, we mark that PR as failed.
        if len(train.prs) == 1:
            pr_id = next(iter(train.prs))

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
            return self._replace(
                builds_in_progress=new_builds,
                open_prs=new_open_prs,
                closed_prs=new_closed_prs,
            )

        new_ps = dict(self.is_good_probabilities.items())

        # Perform the Bayesian update for the is-good probabilities of the PRs
        # involved in this failed train.
        p_train_fails = 1.0 - np.product(
            [self.is_good_probabilities[y] for y in train.prs]
        )
        for x in train.prs:
            p_train_fails_given_x_is_good = np.product(
                [self.is_good_probabilities[y] for y in train.prs if y != x]
            )
            p_x_is_good = self.is_good_probabilities[x]
            p_x_is_good_given_train_failed = (
                p_train_fails_given_x_is_good * p_x_is_good / p_train_fails
            )
            new_ps[x] = p_x_is_good_given_train_failed

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
            state=State.new(),
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

    def start_build_if_possible(self) -> None:
        has_anything_to_build = len(self.state.open_prs) > 0
        has_capacity_to_build = (
            len(self.state.builds_in_progress) < self.config.num_build_slots
        )
        if not (has_anything_to_build and has_capacity_to_build):
            return

        base, prs_in_train = self.strategy(self.state)

        if len(prs_in_train) == 0:
            # Some strategies might conclude they have nothing to do at some
            # point, even if there are open pull requests. For example, because
            # the pull request is already being built.
            return

        train = Train(
            base=base,
            tip=self.allocate_commit(),
            prs=prs_in_train,
        )
        assert all(
            pr in self.state.open_prs for pr in prs_in_train
        ), "Train must consist of currently open PRs."
        assert base == self.state.master_tip or any(
            build.tip == base for build in self.state.builds_in_progress.values()
        ), "Train must build atop master or any currently running build."

        build_id = self.allocate_build_id()
        self.state = self.state.start_build(build_id, train)
        train_is_good = all(self.state.open_prs[pr].is_good for pr in train.prs)
        build_duration = self.rng.normal(
            loc=self.config.avg_build_time, scale=self.config.build_time_stdev
        )
        completion = BuildResult(
            arrived_at=Time(self.t + build_duration),
            id_=build_id,
            did_succeed=train_is_good,
        )
        heapq.heappush(self.events, completion)

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
        self.start_build_if_possible()

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

    # We cut off the graph once the first 5% of runs has finished, because when
    # runs finish, there is no fresh data beyond that point for those runs, so
    # continuing to plot quantiles based on that would be misleading.
    end_time = np.quantile([run.backlog_size_over_time[-1][0] for run in runs], 0.05)
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

    if p50[-1] < 30:
        ax.set_yticks(np.arange(40), minor=True)
    else:
        ax.set_yticks(np.arange(40) * 2, minor=True)

    ax.grid(color="black", linestyle="dashed", axis="y", alpha=0.1, which="both")
    ax.fill_between(
        size_sample_times,
        p25,
        p75,
        alpha=0.2,
        color="black",
        label="p25–p75 backlog size",
    )
    ax.plot(
        size_sample_times,
        p50,
        color="black",
        label="p50 backlog size",
    )
    ax.set_xlabel("time\n(normalized to build time, 1.0 = one build)")
    ax.set_ylabel("number of approved pull requests not yet merged or failed")
    ax.legend()

    wait_times = np.concatenate([run.get_wait_times() for run in runs])
    print(wait_times.shape, wait_times)
    # The scale of the wait times is somewhat arbitrary because it depends on
    # the build time we chose. So normalize everything to the average build
    # time, then we can express time waiting roughly in "number of builds".
    wait_times = wait_times / config.avg_build_time
    mean_wait_time = np.mean(wait_times)
    p50, p90, p97 = np.quantile(wait_times, (0.5, 0.9, 0.97))

    ax = axes[1]
    max_x = max(math.ceil(p97), 3)
    ax.set_xticks(np.arange(max_x), minor=True)
    bins = np.arange(max_x * 2 + 1) * 0.5 - 0.25
    if max_x < 8:
        bins = np.arange(max_x * 4 + 1) * 0.25 - 0.125

    ax.grid(color="black", linestyle="dashed", axis="x", alpha=0.1, which="both")
    ax.hist(wait_times, bins=bins, color="black", alpha=0.2)

    ax.axvline(
        x=mean_wait_time,
        color="red",
        label=f"mean wait time ({mean_wait_time:.2f})",
    )
    p50, p90 = np.quantile(wait_times, (0.5, 0.9))
    ax.axvline(
        x=p50,
        color="black",
        label=f"p50 wait time ({p50:.2f})",
    )
    ax.axvline(
        x=p90,
        color="black",
        linestyle="dotted",
        label=f"p90 wait time ({p90:.2f})",
    )
    ax.set_xlabel(
        "wait time until merge or fail\n(normalized to build time, 1.0 = one build)"
    )
    ax.set_ylabel("number of pull requests")
    ax.legend()

    plt.tight_layout()
    plt.savefig(f"out/{config.name}_{strategy_name}.png", dpi=400)


def strategy_classic(state: State) -> Tuple[Commit, set[PrId]]:
    """
    The "classic" stategy implemented by Hoff. Only supported when
    NUM_BUILD_SLOTS = 1, i.e. no build parallelism. This strategy builds
    singleton trains, trying the lowest-numbered PR first.
    """
    assert (
        len(state.builds_in_progress) == 0
    ), "This strategy does not support parallelism."
    base = state.master_tip
    candidate = min(state.open_prs.keys())
    return base, {candidate}


def strategy_fifo(state: State) -> Tuple[Commit, set[PrId]]:
    """
    Like classic, but build the least recently approved pull request first. Only
    builds one pull request at a time, does not support parallelism.
    """
    assert (
        len(state.builds_in_progress) == 0
    ), "This strategy does not support parallelism."
    base = state.master_tip
    candidates = sorted((pr.arrived_at, pr.id_) for pr in state.open_prs.values())
    return base, {candidates[0][1]}


def strategy_lifo(state: State) -> Tuple[Commit, set[PrId]]:
    """
    Similar to classic, but build the most recently approved pull request first.
    I.e. keep a stack, not a queue. Most people consider this unfair, but it
    makes many people happier at the cost of making a few very unhappy.
    """
    assert (
        len(state.builds_in_progress) == 0
    ), "This strategy does not support parallelism."
    base = state.master_tip
    candidates = sorted((-pr.arrived_at, pr.id_) for pr in state.open_prs.values())
    return base, {candidates[0][1]}


def strategy_classic_parallel(state: State) -> Tuple[Commit, set[PrId]]:
    """
    Build the pull request with the lowest PR id first. If there are builds in
    progress, optimistically try to build on top, but only one PR at a time,
    i.e. no rollups.
    """
    base = state.master_tip
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
        return base, {}

    return base, {min(candidates)}


def main() -> None:
    cfgs = (Config.subcritical(), Config.critical(), Config.supercritical())
    # cfgs = (Config.subcritical(),)
    for cfg in cfgs:
        runs = []
        for seed in range(200):
            sim = Simulator.new(
                seed=seed,
                config=cfg,
                strategy=strategy_classic_parallel,
            )
            sim.run_to_last_pr()
            runs.append(sim)

        plot_results(cfg, "classic_parallel", runs)


if __name__ == "__main__":
    main()
