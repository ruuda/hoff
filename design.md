# Design

This is a draft design of a minimum viable collaboration system that integrates
with GitHub and Travis CI.

## Surface area

To interact with the outside world, the system will need:

 * An http server to receive GitHub and Travis CI webhook calls.
 * An http client to call the GitHub Travis CI APIs.
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
