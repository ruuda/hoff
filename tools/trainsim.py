#!/usr/bin/env python3

"""
For testing https://github.com/channable/hoff/issues/77#issuecomment-1179430191
"""

from __future__ import annotations

import matplotlib  # type: ignore
import numpy as np
import heapq

from dataclasses import dataclass
from matplotlib import pyplot as plt  # type: ignore
from matplotlib.font_manager import FontProperties  # type: ignore
from numpy.random import Generator
from numpy.typing import ArrayLike
from typing import Callable, NamedTuple, NewType, Tuple

# When we set the mean build time to the average time between PRs, we are at
# that critical point where on average the system can keep up and still merge
# everything, but there will be spans of time where we are unlucky and a backlog
# builds up. Being *right* at that critical point is bad because once there is
# some backlog, it has as much probability of growing as it has of shrinking,
# but it can't shrink below zero, so over time it does grow. So let's say we are
# not yet at that point, and the time between PRs is a bit more than the build
# time.
AVG_TIME_BETWEEN_PRS = 10.0
AVG_TIME_TO_APPROVE = 60.0
AVG_BUILD_TIME = 9.0
BUILD_TIME_STDDEV = 1.0
PROBABILITY_PR_IS_GOOD = 0.9
NUM_BUILD_SLOTS = 1


Time = NewType("Time", float)
Commit = NewType("Commit", int)
PrId = NewType("PrId", int)
BuildId = NewType("BuildId", int)


class PullRequest(NamedTuple):
    arrived_at: Time
    id_: PrId
    is_good: bool


class BuildResult(NamedTuple):
    arrived_at: Time
    id_: BuildId
    did_succeed: bool


def generate_pr_events(rng: Generator, num_prs: int) -> list[PullRequest]:
    """
    Generate arrival events for pull requests, in no particular order.
    """

    # We model incoming PRs as a Poisson process, which means the inter-arrival
    # times follow an exponential distribution.
    times_between_open = rng.exponential(scale=AVG_TIME_BETWEEN_PRS, size=num_prs)

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
        scale=AVG_TIME_TO_APPROVE / 3,
        size=num_prs,
    )
    is_good = rng.uniform(size=num_prs) < PROBABILITY_PR_IS_GOOD

    t_open = np.cumsum(times_between_open)
    t_approve = t_open + times_to_approve

    return [
        PullRequest(
            arrived_at=Time(t_approve[i]),
            id_=PrId(i),
            is_good=is_good[i],
        )
        for i in range(0, num_prs)
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
    strategy: Strategy
    rng: Generator
    t: Time
    last_pr_received_at: Time
    state: State
    events: list[PullRequest | BuildResult]
    backlog_size_over_time: list[Tuple[Time, int]]
    next_available_commit: Commit
    next_available_build: BuildId

    @staticmethod
    def new(seed: int, num_prs: int, strategy: Strategy) -> Simulator:
        rng = np.random.default_rng(seed=seed)
        events: list[PullRequest | BuildResult] = [
            evt for evt in generate_pr_events(rng, num_prs)
        ]
        heapq.heapify(events)
        return Simulator(
            strategy=strategy,
            rng=rng,
            t=Time(0.0),
            last_pr_received_at=Time(0.0),
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
        has_capacity_to_build = len(self.state.builds_in_progress) < NUM_BUILD_SLOTS
        if not (has_anything_to_build and has_capacity_to_build):
            return

        base, prs_in_train = self.strategy(self.state)
        train = Train(
            base=base,
            tip=self.allocate_commit(),
            prs=prs_in_train,
        )
        assert all(
            pr in self.state.open_prs for pr in prs_in_train
        ), "Train must consist of currently open PRs."
        assert len(prs_in_train) > 0, "Train must build at least one PR."
        assert base == self.state.master_tip or any(
            build.tip == base for build in self.state.builds_in_progress.values()
        ), "Train must build atop master or any currently running build."

        build_id = self.allocate_build_id()
        self.state = self.state.start_build(build_id, train)
        train_is_good = all(self.state.open_prs[pr].is_good for pr in train.prs)
        build_duration = self.rng.normal(loc=AVG_BUILD_TIME, scale=BUILD_TIME_STDDEV)
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
            self.last_pr_received_at = event.arrived_at

        if isinstance(event, BuildResult):
            if event.did_succeed:
                self.state = self.state.complete_build_success(self.t, event.id_)
            else:
                self.state = self.state.complete_build_failure(self.t, event.id_)

        self.backlog_size_over_time.append((self.t, len(self.state.open_prs)))
        self.start_build_if_possible()

    def run_to_completion(self) -> None:
        while len(self.events) > 0:
            self.handle_single_event()

        assert len(self.state.open_prs) == 0
        assert len(self.state.builds_in_progress) == 0

    def get_backlog_trace(self, ts: ArrayLike) -> ArrayLike:
        """
        Sample the size of the backlog at the given times. This computes the
        time-weighted average size over the intervals defined by `ts`. The times
        `ts` must be increasing.
        """
        result = []
        last_size = 0
        i = 0
        max_i = len(self.backlog_size_over_time)
        prev_t = 0.0

        for t in np.nditer(ts):
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


def plot_results(runs: list[Simulator]) -> None:
    font = FontProperties()
    font.set_family("Source Serif Pro")
    matplotlib.rcParams["font.family"] = font.get_name()

    fig, axes = plt.subplots(nrows=1, ncols=2, tight_layout=True, figsize=(15, 5))

    ax = axes[0]

    last_pr_received = np.quantile([
        run.last_pr_received_at for run in runs
    ], 0.05)
    end_time = np.quantile([
        run.backlog_size_over_time[-1][0]
        for run in runs
    ], 0.5)
    size_sample_times = np.linspace(0.0, end_time, num=200)

    backlog_sizes = np.array([
        run.get_backlog_trace(size_sample_times) for run in runs
    ])
    # Even after we sample the backlog as a time-weighted value in the buckets,
    # the lines still look quite noisy, smooth that out a bit by averaging over
    # some time steps.
    window_len = 10
    tmp = np.cumsum(backlog_sizes, axis=1)
    tmp[:, window_len:] = tmp[:, window_len:] - tmp[:, :-window_len]
    backlog_sizes = tmp[:, window_len - 1:] / window_len
    size_sample_times = size_sample_times[window_len - 1:]

    p25, p50, p75 = np.quantile(backlog_sizes, (0.1, 0.5, 0.9), axis=0)
    ax.set_yticks(np.arange(10), minor=True)
    ax.grid(color="black", linestyle="dashed", axis="y", alpha=0.1, which="both")
    ax.fill_between(
        size_sample_times, p25, p75,
        alpha=0.2,
        color="black",
        label="p25–p75 backlog size"
    )
    ax.plot(
        size_sample_times, p50,
        color="black",
        label="p50 backlog size",
    )
    ax.axvline(x=last_pr_received,
            color="red",
            label="p05 last PR received",
    )
    ax.set_xlabel("time")
    ax.set_ylabel("number of approved pull requests not yet merged or failed")
    ax.legend()

    wait_times = np.concatenate([run.get_wait_times() for run in runs])
    # The scale of the wait times is somewhat arbitrary because it depends on
    # the build time we chose. So normalize everything to the average build
    # time, then we can express time waiting roughly in "number of builds".
    wait_times = wait_times / AVG_BUILD_TIME
    ax = axes[1]
    ax.set_xticks(np.arange(25), minor=True)
    ax.grid(color="black", linestyle="dashed", axis="x", alpha=0.1, which="both")
    ax.hist(wait_times, bins=np.arange(50) * 0.5 - 0.25, color="black", alpha=0.2)

    mean_wait_time = np.mean(wait_times)
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
    ax.set_xlabel("wait time until merge or fail\n(normalized to build time, 1.0 = one build)")
    ax.set_ylabel("number of pull requests")
    ax.legend()

    plt.tight_layout()
    plt.savefig("trainsim.png", dpi=400)


def main() -> None:
    runs = []
    for seed in range(200):
        sim = Simulator.new(
            seed=seed,
            num_prs=200,
            strategy=strategy_classic,
        )
        sim.run_to_completion()
        runs.append(sim)

    plot_results(runs)


if __name__ == "__main__":
    main()
