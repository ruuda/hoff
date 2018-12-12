# Background

I’ve worked on several codebases with several source control systems. Big
repositories with more than a thousand active developers and ~300 commits per
day. Small repositories where all developers are in the same room. My personal
projects, where I am the sole developer apart from the occasional pull request.
I’ve worked in codebases with sophisticated review systems and testing
infrastructure. I’ve worked in codebases where there was no code review at all,
and no continuous integration. (Sometimes not even tests.) No one size fits all,
though some workflows are definitely better than others. Here I outline some of
my thoughs on collaborating on a software system.

What I am proposing here is a system that manages source control, code review,
and continuous integration. I don’t think there is a name for such a system yet.
I’ll use the term *Collaboration System* (CS) henceforth.

## The Not Rocket Science Principle

I firmly believe in the [Not Rocket Science Principle][not-rocket-science]:

> Automatically maintain a repository of code that always passes all the tests.

This implies that you have a test suite in the first place. While I am generally
sceptic about tests (I think a strong type system can be much more valuable than
a big test suite) -- and I certainly don’t believe in test driven development --
I do believe in tests. Ideally, a passing test suite should give you enough
confidence to deploy or release the product.

In terms of source control (I’ll assume Git here), this means that the master
branch should always pass all the tests. You should always be confident to
deploy or make a release from master. If this is to be automatically enforced,
that means no pushing to master. Which raises the question: how do commits end
up in master?

[not-rocket-science]: https://graydon2.dreamwidth.org/1597.html

## Proposing changes

In a distributed world, it is obvious how changes are developed: you pull from
master, and build your changes on top. Whether locally you do that in one or
more feature branches, or directly on top of master, is irrelevant. In the end,
your proposal is a single commit. Its chain of parents eventually points to some
commit in the master branch: the point where you branched off.

The collaboration system takes such a proposed commit as input. Possibly it
waits for the changes to be approved by a reviewer. It integrates the changes
with the current master branch, runs the tests, and if they pass, forwards
master to the new version. This raises two new questions: how are reviews
handled, and how are changes integrated into master?

## Code review policy

Code review is intimately related to trust. Different projects require different
approaches here. When the people you work with are all in the same room, you
probably trust them. If there is an issue you can just walk up to them and ask.
In a big project, you might not know all of your colleagues. You might not even
live on the same continent. Some kind of trust hierarchy is needed there. If you
allow contributions from outside (e.g. pull requests on GitHub), you don’t want
any random person on the internet to be able to make changes without review.

For a small project where trust is high, a simple full access model can be
sufficient. Ask for a review when it makes sense, but don’t waste valuable
developer time by asking somebody to review your whitespace fixes. Perhaps even
significant code is not always reviewed. (I’ve had an employer who thought it
was too expensive to have two programmers look at the same code. While I
disagree, the decision was not mine to make.) The fact that malicious code can
be pushed without review is not an issue when trust is high.

Sometimes all you care about is that *somebody* apart from the author looked at
the code. For mission-critical code, you might require a review from multiple
designated reviewers. There are countless policies out there. It depends on the
project which ones are suitable.

Trust and code review are matters of policy, not technology. The collaboration
system doesn’t care. It must be flexible enough to support various policies, but
ultimately all it cares about is a valid LGTM stamp on a proposed change.

## Integrating changes and the history

(This section is a work in progress.) Keep the history clean. That probably
means linear, so rebase. Enforce commit message format. (It is trivial to do if
changes are guarded, so why not?) Every commit should at least compile, but
perhaps it need not pass all the tests.

## Dependent changes

(This section is a work in progress.) This is where all tools I know of fall
short. Rietveld has dependent patchsets, but the local Git workflow is still a
mess. Changes are a DAG, and every change might consist of multiple commits. If
feature B depends on A, and the reviewer for A asked you to make a change on top
of A, then now you need to rebase B. With a moderately complex dependency graph
this becomes a mess. And Git doesn’t track dependencies. (This is the single
point where the Team Foundation branching model -- which seemed ridiculous to me
at first -- has an advantage over Git.) Need a tool that can track dependencies
in the review (unlike GitHub, which just shows you the diff including
dependencies), but also issue the right Git rebase commands.

## The repository

(This section is a work in progress.)

 * The set of projects that you wish to be able to make atomic changes
   to should live in the same repository. Even if they are decoupled from a code
   point of view (i.e. you can build and test them independently), they could
   still be coupled via e.g. a REST API, dependence on a database schema, or
   using a specific file format.

 * Creates tension with throughput. Test *every* project again if you only made
   a change to one?

## Similar projects and further reading

There exists lots of sotware that deals with code review and continuous
integration already, but no existing project offered all of the things I wanted.
Below are some of the projects that inspired this project:

 * [Rietveld][rietveld], a great code review tool. It is mature and has it has
   many features, but it can only deal with dependent patchsets in a limited
   way, and it does not enforce the Not Rocket Science Principle. It creates a
   linear history, but it requires custom tools to interact with it, different
   from normal Git workflows. Rietveld was itself based on [Mondrian][mondrian].
 * [Gerrit][gerrit], a code review tool for Git based on Rietveld. It can rebase
   proposed changes, but as far as I am aware, it cannot enforce the Not Rocket
   Science Principle.
 * [Iron][iron], a code review and release management tool. It was released as
   source-available by Jane Street, with an interesting trilogy of blog posts
   ([I][iron-i], [II][iron-ii], [III][iron-iii]), but unfortunately is lacks any
   further documentation. I have no clue how to build or use it. The blog posts
   are a good read nevertheless.
 * [Bors][bors], a bot for GitHub that enforces the Not Rocket Science
   Principle, written for the Rust project. Graydon’s [post about the Not Rocket
   Science Principle][not-rocket-science] provides a bit of background. Bors is
   not a code review tool, it only handles gating commits on test results. Its
   integration strategy is to do a merge, which creates an ugly history. The
   original implementation of Bors did not scale very well, so a more robust
   rewrite called [Homu][homu] was created.
 * [Zuul][zuul], a commit queue that speculatively starts builds for changes to
   be integrated after changes that are being tested. When builds usually pass,
   this allows for higher throughput.

[bors]:     https://github.com/graydon/bors
[gerrit]:   https://www.gerritcodereview.com/
[homu]:     https://github.com/servo/homu
[iron-i]:   https://blogs.janestreet.com/code-review-that-isnt-boring/
[iron-ii]:  https://blogs.janestreet.com/scrutinizing-your-code-in-style/
[iron-iii]: https://blogs.janestreet.com/ironing-out-your-release-process/
[iron]:     https://github.com/janestreet/iron
[mondrian]: https://www.youtube.com/watch?v=sMql3Di4Kgc
[rietveld]: https://github.com/rietveld-codereview/rietveld
[zuul]:     https://zuul-ci.org/
