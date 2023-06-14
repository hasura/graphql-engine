This note is in [Hasura.App.State](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/App/State.hs#L65).
It is referenced at:
  - line 527 of [Hasura.App](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/App.hs#L527)
  - line 27 of [Hasura.RQL.DDL.Schema.Cache.Config](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/Schema/Cache/Config.hs#L27)
  - line 64 of [Hasura.RQL.DDL.Schema.Cache.Config](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/RQL/DDL/Schema/Cache/Config.hs#L64)

# Hasura Application State

Hasura Application state represents the entire state of hasura.

Hasura Application State = AppEnv (static) + AppContext (dynamic)

Hasura Application State can be divided into two parts:

  1. Read-Only State (Static State):
  =================================
  The information required to build this state is provided only during the
  initialization of hasura. This information is immutable. If you want update any
  field in this state, you would need to shutdown the current instance and
  re-launch hausura with new information.

  Eg: If you want to run hasura in read-only mode, you would have to mention
      this information when hasura starts up. There is no way to make hasura
      run in read-only mode once it has booted up.

  2. Runtime Configurable State (Dynamic State):
  ==============================================
  The information present in this state can be updated during the runtime. This state
  is mutable and does not require a restart of hasura instance to take effect.

  The fields in the state are usually updated via Metadata API's or Hasura Console.

  Eg: You can change the entries in Allowlist via console and hasura need not restart
      for the changes to take effect.


