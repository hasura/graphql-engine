# Data Connector Agent Tests
This test suite provides a set of tests that is able to test any Data Connector agent that contains the Chinook data set to ensure the agent is behaving as expected. The test executable is designed to be distributable to customers building Data Connector agents, but is also useful to ensure Hasura's own agents are working correctly.

Not all tests will be appropriate for all agents. Agents self-describe their capabilities and only the tests appropriate for those capabilities will be run.

The executable also has the ability to export the OpenAPI spec of the Data Connector agent API so that customers can use that to ensure their agent complies with the API format. In addition, the Chinook data set can be exported to files on disk in various formats.

## How to Use
### Running Tests
First, start your Data Connector agent and ensure it is populated with the [Chinook data set](https://github.com/lerocha/chinook-database/). For example, you could start the Reference Agent by following the instructions in [its README](../../dc-agents/reference/README.md).

To run the tests against the agent (for example), you must specify the agent's URL on the command line (`--agent-base-url`), as well as the agent's configuration JSON (`--agent-config`, sent in the `X-Hasura-DataConnector-Config` header):

```
cabal run test:tests-dc-api -- test --agent-base-url "http://localhost:8100" --agent-config '{}'
```

The test suite will discover what capabilities the agent has by querying it. It will then tailor the tests that it will run to match only those capabilities that the agent has said it supports.

If your agent supports the datasets capability, you can omit the `--agent-config` argument and the test suite will clone the Chinook dataset template on the agent to run its test against. If you need to specify some additional configuration to be added to the cloned dataset's configuration, you can specify it using `--merge-agent-config`.

The test suite is implemented using the [Sandwich](https://codedownio.github.io/sandwich/) test framework. The standard Sandwich command line arguments can be passed by suffixing your command line with `sandwich` and then all following args will be passed to Sandwich.

For example, to run the Terminal UI mode of Sandwich, you could run:

```
cabal run test:tests-dc-api -- test --agent-base-url "http://localhost:8100" --agent-config '{}' sandwich --tui
```

By default Sandwich will write test results into a `test_runs` folder. Every test has a folder that will contain debug information, for example:
- All the HTTP requests that the test made to the agent (`agent-request-[n].http`). These files can be used with a client such as [REST Client (VSCode)](https://github.com/Huachao/vscode-restclient) or [HTTP Client (IntelliJ)](https://www.jetbrains.com/help/idea/http-client-in-product-code-editor.html)
- All the HTTP responses from the agent that matched those requests (`agent-response-[n].http`)

### Exporting Data
To export the Data Connector Agent OpenAPI spec, you can run this command, and the spec will be written to stdout.

```
> cabal run test:tests-dc-api -- export-openapi-spec
```

To export the Chinook data set, you can run this command:
```
> cabal run test:tests-dc-api -- export-data -d /tmp/chinook-data -f JSONLines
```

This will export the data into the directory specified by `-d` in the `JSONLines` format (`-f`) which is as a JSON object per row, newline separated. Each table's data will be exported into a separate file.

If you need to customize the format of any DateTime columns, you can use the `--datetime-format` option and specify a format string using the syntax specified [here](https://hackage.haskell.org/package/time-1.12.2/docs/Data-Time-Format.html#v:formatTime). By default the DateTime columns are exported in ISO8601 format.

The other format supported by `-f` is `JSON`, which results in each file being a JSON array of table rows as JSON objects.
