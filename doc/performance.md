# Performance

What kind of performance should I be aiming for, for this to be usable in a big
project? Let’s assume a project that approves roughly 300 changes per day, where
every proposed change involves about 30 events. Events can be pushes, build
status notifications, comments, and opening or closing a proposal. (These
numbers are in the ballpark of the main repository of the Chromium project,
although the number of events is just an estimate.) **So let’s define a
*big project* as a project that generates 9000 events per day.**

Even for an international effort, there will be peek hours. As a very rough
estimate (I don’t have any actual data), the 80/20 rule dictates that 80% of the
events happen in 20% of the time. That is 7200 events in 288 minutes during peek
hours, 25 events per minute.

Assume events are generated by a Poisson process with a rate of 25 events per
minute. We will compute the interval _x_, such that with 99% probability the
time between consecutive events is larger than _x_. In R:

    > qexp(0.01, rate = 25/60)
    [1] 0.02412081

That gives us about 24 milliseconds to respond to a request when running on a
single core. Responing to a request might involve enqueueing some work to handle
the event, and actually doing that work may take longer, as long as we can
handle a sustained load of 25 events per minute. If handling the event involves
doing a Git pull, it can take a few seconds, but to decide what to do with a
comment, 24 milliseconds should be plenty.

In my last benchmark, the server could handle requests at 1.96 ± 1.5 ms per
request on a single core, so no blockers there.
