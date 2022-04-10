# GraphQL Data Wrappers Agent Tests
This test suite provides a set of tests that is able to test any GDW agent that contains the Chinook data set to ensure the agent is behaving as expected. The test executable is designed to be distributable to customers building GDW agents, but is also useful to ensure Hasura's own agents are working correctly.

Not all tests will be appropriate for all agents. Agents self-describe their capabilities and only the tests appropriate for those capabilities will be run.

The executable also has the ability to export the OpenAPI spec of the GDW agent API so that customers can use that to ensure their agent complies with the API format.

## How to Use
First, start your GDW agent and ensure it is populated with the Chinook data set. For example, you could start the Reference Agent by following the instructions in [its README](../../gdw-agents/reference/README.md).

To run the tests against the agent (for example), you must specify the agent's URL on the command line:

```
cabal run test:tests-gdw-api -- test -u "http://localhost:8100"
```

By default, the test suite will discover what capabilities the agent exposes by querying it. Otherwise, the user can use command line flags to specify which capabilities their agent has to ensure that it exposes the expected capabilities and that the test suite only runs the tests that correspond to those capabilities.

To set the agent's available the capabilities use `-c` and comma separate them:

```
> cabal run test:tests-gdw-api -- test -u "http://localhost:8100" -c relationships
```

If `-c` is omitted, the default value is `autodetect`. If you have no capabilities, you can specify `none`.

To export the OpenAPI spec, you can run this command, and the spec will be written to stdout.

```
> cabal run test:tests-gdw-api -- export-openapi-spec
```
