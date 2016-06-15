# Approach

In an ideal world, I would have the time and motivation to build an integrated
system that sports all of the following:

 1. A Git server.
 2. Something that keeps track of interdependent proposed changes.
 3. A sensible code review UI that can deal with rebasing.
 4. A system to enforce history policy.
 5. A system to enforce review policy.
 6. A system to enforce automated testing policy.
 7. A gatekeeper that integrates approved changes into master.

With sufficient feature creap, a bug tracker, repository browser, and chat
server slip in, and after a while you end up with a complex system that can do
everything, but does nothing very well. While some features can be valuable when
integrated properly, they won’t be there from the start. An external repository
browser, bug tracker, and build and test runner are still required.

## Integration with existing tools

There are numerous external systems that could fill the gaps: Gitiles or Cgit to
browse the repository; Bugzilla, Mantis, or Monorail for bug tracking; Jenkins
or Buildbot to run builds and tests. But to set up and host these systems is
nontrivial. Perhaps more importantly, self-hosted solutions like these are a
treshold for contributors in a GitHub-dominated world. This is true even for
more integrated tools: nobody is going to sign up at your Phabricator or GitLab
instance. There is a strong network effect.

## GitHub and Travis CI

Modularity is important, but I need to start somewhere. I think integrating with
GitHub and Travis CI is the best route to a minimal viable system. It is far
from ideal:

 * There is no simple way to prevent merging from the GitHub UI. The entire
   point is to have a gatekeeper that enforces policy, but building on top of
   GitHub means that it is easy to accidentally skip all the safety nets.

 * GitHub’s review system is rudimentary. There is no such thing as a dependent
   pull request, and nothing relates rebased commits to their original ones.

While my system would act as a gatekeeper, and keep track of dependencies
between proposed changes, building on top of GitHub’s pull requests will make
this a half-baked experience at best. Still, it should not be worse than only
using GitHub, and a half-baked experience is better than nothing at all.

By building on top of GitHub, I get a lot for free too:

 * A Git server.
 * A repository browser.
 * A review tool. Rudimentary as it is, I don’t have to build it myself.
 * A workflow for proposing changes (pull requests).
 * An authentication system to which millions of developers can log in already.

For building and running tests, I’ll use Travis CI. It is simple, solid, and it
is good at what it does. Perhaps for a complex project more control over
scheduling and such would be useful, and a more integrated UI could be nice. But
all I really need is something that I can pass a commit hash to, and that tells
me pass or fail a while later.
