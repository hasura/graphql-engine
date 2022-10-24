# Data Connector Agent Tests
This test suite provides a set of tests that is able to test any Data Connector agent that contains the Chinook data set to ensure the agent is behaving as expected. The test executable is designed to be distributable to customers building Data Connector agents, but is also useful to ensure Hasura's own agents are working correctly.

Not all tests will be appropriate for all agents. Agents self-describe their capabilities and only the tests appropriate for those capabilities will be run.

The executable also has the ability to export the OpenAPI spec of the Data Connector agent API so that customers can use that to ensure their agent complies with the API format. In addition, the Chinook data set can be exported to files on disk in various formats.

## How to Use
First, start your Data Connector agent and ensure it is populated with the [Chinook data set](https://github.com/lerocha/chinook-database/). For example, you could start the Reference Agent by following the instructions in [its README](../../dc-agents/reference/README.md).

To run the tests against the agent (for example), you must specify the agent's URL on the command line (`-u`), as well as the agent's configuration JSON (`-s`, sent in the `X-Hasura-DataConnector-Config` header):

```
cabal run test:tests-dc-api -- test -u "http://localhost:8100" -s '{}'
```

By default, the test suite will discover what capabilities the agent exposes by querying it. Otherwise, the user can use command line flags to specify which capabilities their agent has to ensure that it exposes the expected capabilities and that the test suite only runs the tests that correspond to those capabilities.

To set the agent's available the capabilities use `-c` and comma separate them:

```
> cabal run test:tests-dc-api -- test -u "http://localhost:8100" -s '{}' -c relationships
```

If `-c` is omitted, the default value is `autodetect`. If you have no capabilities, you can specify `none`.

To export the OpenAPI spec, you can run this command, and the spec will be written to stdout.

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
