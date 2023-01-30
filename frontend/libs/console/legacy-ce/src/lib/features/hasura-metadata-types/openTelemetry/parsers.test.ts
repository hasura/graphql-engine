import {
  parseUnexistingEnvVarSchemaError,
  parseHasuraEnvVarsNotAllowedError,
} from './parsers';

// Testing parseUnexistingEnvVarSchemaError is important until it's based on the custom regex
describe('parseUnexistingEnvVarSchemaError', () => {
  it('When invoked with a "unexistingEnvVar" error, then should return it', () => {
    const theRealServerError = {
      code: 'unexpected',
      error: 'cannot continue due to new inconsistent metadata',
      internal: [
        {
          definition: {
            headers: [{ name: 'foo', value_from_env: 'baz' }],
            otlp_traces_endpoint: 'http://example.io',
            protocol: 'http/protobuf',
            resource_attributes: [],
          },
          name: 'open_telemetry exporter_otlp',
          reason: "Inconsistent object: environment variable 'baz' not set",
          type: 'open_telemetry',
        },
      ],
      path: '$.args',
    };

    const theErrorTheConsoleMatters = {
      internal: [
        {
          reason: "Inconsistent object: environment variable 'baz' not set",
        },
      ],
    };

    const result = parseUnexistingEnvVarSchemaError(theRealServerError);
    expect(result).toEqual({
      success: true,
      data: theErrorTheConsoleMatters,
    });
  });
});

// Testing parseHasuraEnvVarsNotAllowedError is important until it's based on the custom regex
describe('parseHasuraEnvVarsNotAllowedError', () => {
  it('When invoked with a "hasuraEnvVarsNotAllowed" error, then should return it', () => {
    const theRealServerError = {
      code: 'parse-failed',
      error:
        'env variables starting with "HASURA_GRAPHQL_" are not allowed in value_from_env: HASURA_GRAPHQL_ENABLED_APIS',
      path: '$.args.exporter_otlp.headers[1]',
    };

    const theErrorTheConsoleMatters = {
      error:
        'env variables starting with "HASURA_GRAPHQL_" are not allowed in value_from_env: HASURA_GRAPHQL_ENABLED_APIS',
    };

    const result = parseHasuraEnvVarsNotAllowedError(theRealServerError);
    expect(result).toEqual({
      success: true,
      data: theErrorTheConsoleMatters,
    });
  });
});
