#!/usr/bin/env python3

"""
For testing https://github.com/channable/hoff/issues/77#issuecomment-1179430191
"""

from __future__ import annotations

import numpy as np
import heapq

from dataclasses import dataclass
from typing import NamedTuple, NewType
from numpy.random import Generator

# When we set the mean build time to the average time between PRs, we are at
# that critical point where on average the system can keep up and still merge
# everything, but there will be spans of time where we are unlucky and a backlog
# builds up.
AVG_TIME_BETWEEN_PRS = 10.0
AVG_TIME_TO_APPROVE = 60.0
AVG_BUILD_TIME = 10.0
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


@dataclass(frozen=False)
class Simulator:
    rng: Generator
    state: State
    events: list[PullRequest | BuildResult]
    next_available_commit: Commit
    next_available_build: BuildId

    @staticmethod
    def new(seed: int, num_prs: int) -> Simulator:
        rng = np.random.default_rng(seed=seed)
        events: list[PullRequest | BuildResult] = [
            evt for evt in generate_pr_events(rng, num_prs)
        ]
        heapq.heapify(events)
        return Simulator(
            rng=rng,
            state=State.new(),
            events=events,
            next_available_commit=Commit(1),
            next_available_build=BuildId(0),
        )


def main() -> None:
    sim = Simulator.new(seed=0, num_prs=100)
    for evt in sim.events:
        print(evt.arrived_at, evt)


if __name__ == "__main__":
    main()
