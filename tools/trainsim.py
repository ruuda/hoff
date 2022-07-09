#!/usr/bin/env python3

"""
For testing https://github.com/channable/hoff/issues/77#issuecomment-1179430191
"""

import numpy as np

from typing import NamedTuple
from numpy.random import Generator

# When we set the mean build time to the average time between PRs, we are at
# that critical point where on average the system can keep up and still merge
# everything, but there will be spans of time where we are unlucky and a backlog
# builds up.
avg_time_between_prs = 10.0
avg_time_to_approve = 60.0
mean_build_time = 10.0
build_time_stdev = 1.0
probability_pr_is_good = 0.9


class PrApproved(NamedTuple):
    id_: int
    is_good: bool


class BuildCompleted(NamedTuple):
    id_: int
    did_pass: bool

Event = PrApproved | BuildCompleted


class TimedEvent(NamedTuple):
    t: float
    event: Event


def generate_pr_events(
    rng: Generator,
    num_prs: int,
) -> list[TimedEvent]:

    # We model incoming PRs as a Poisson process, which means the inter-arrival
    # times follow an exponential distribution.
    times_between_open = rng.exponential(scale=avg_time_between_prs, size=num_prs)

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
        scale=avg_time_to_approve / 3,
        size=num_prs,
    )
    is_good = rng.uniform(size=num_prs) < probability_pr_is_good

    t_open = np.cumsum(times_between_open)
    t_approve = t_open + times_to_approve

    events = [
        TimedEvent(t_approve[i], PrApproved(i, is_good[i]))
        for i in range(0, num_prs)
    ]
    events.sort()

    return events


def main() -> None:
    rng = np.random.default_rng(seed=0)
    events = generate_pr_events(rng, 100)
    for evt in events:
        print(evt.t, evt)


if __name__ == "__main__":
    main()
