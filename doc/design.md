# Design

This is a draft design of a minimum viable collaboration system that integrates
with GitHub and Travis CI.

NOTE: This document is outdated.
TODO: Update this.

## Surface area

To interact with the outside world, the system will need:

 * An http server to receive GitHub and Travis CI webhook calls.
 * An http client to call the GitHub and Travis CI APIs.
 * An http server to serve a webpage with the current status.
 * A way to read configuration.
 * A way to persist state to disk and reload in case of a restart.

Configuration does not need to be dynamic for now, so a simple configuration
file which is read at startup will suffice.

Persistence is a harder problem. Doing persistence properly is hard, and
databases and things like Redis have already solved the hard parts, so it seems
attractive to rely on something external. For an initial version it is probably
fine to serialize and write the state at shutdown and read and deserialize it at
startup, but for a serious product durability is a must-have. There are two
issues here:

 * Do I want to use an external system for persistence? It might relieve me from
   the burden of guaranteeing durability, but it increases setup complexity
   **a lot**.

 * Even if I eventually want to have durable state, can I get away with
   periodic serialization in a prototype? Will it be difficult to change this
   later on, and will persistence be too much work for a throwaway prototype?

For now I’ll go with serialization as simple as possible; write on shutdown and
read at startup. There will be no durability initially.

## State

What state does the system need to keep track of? First of all, the things that
are configuration rather than state:

 * The url of the repository to keep track of (let’s limit it to one initially).
 * API tokens and urls.
 * Authorization for review policy etc. will come later.

As for the actual state, I need to know:

 * The set of open pull request, and whether they have been approved or not.
   This could be recovered entirely from GitHub.

 * The state per approved pull request: approved but missing enforced checks,
   build pending or failed, etc. This cannot easily be recovered from GitHub.

 * The current candidate pull request for integrating. Also the sha of the
   integrated change (rebased or merge commit, depending on the strategy), and
   details about the build state.

## Events

What are the events that can change the state?

 * A pull request was opened. This should make the commits elegible for
   automated checks (e.g. enforce commit message format), but it should not
   attempt a build or merge.

 * A comment with magic approve incantation was left on an open pull request.
   This makes the pull request elegible for building and testing if all checks
   passed.

 * The `HEAD` of a pull request changed. This may throw away the details of the
   previous revision and treat the new one as a fresh pull request. That way
   approval and check results are invalidated automatically. If check results
   are made available via the web interface only (and not by leaving a comment
   on GitHub), this is sufficient.

 * A pull request was closed. Removes the pull request from the system.

 * The CI build state changed. If this concerned the current integration
   candidate, proceed to the next step. If the build and tests were successful,
   fast-forward master and consider the next candidate. If it failed, mark the
   pull request as failed (until it is modified) and consider the next candidate
   without updating master.

 * Perhaps there should be an option to retry a failed build.
