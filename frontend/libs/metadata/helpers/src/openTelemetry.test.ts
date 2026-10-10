import {
  parseOpenTelemetry,
  parseUnexistingEnvVarSchemaError,
  parseHasuraEnvVarsNotAllowedError,
} from './openTelemetry';

const validOpenTelemetry = {
  status: 'enabled',
  data_types: ['traces'],
  batch_span_processor: { max_export_batch_size: 512 },
  exporter_otlp: {
    headers: [{ name: 'x-foo', value: 'bar' }],
    protocol: 'http/protobuf',
    resource_attributes: [],
    traces_propagators: ['b3'],
    otlp_traces_endpoint: 'http://example.io',
  },
};

describe('parseOpenTelemetry', () => {
  it('succeeds for a valid, enabled config with a traces endpoint', () => {
    const result = parseOpenTelemetry(validOpenTelemetry);
    expect(result.success).toBe(true);
  });

  it('fails when trace export is enabled but the endpoint is missing', () => {
    const result = parseOpenTelemetry({
      ...validOpenTelemetry,
      exporter_otlp: {
        ...validOpenTelemetry.exporter_otlp,
        otlp_traces_endpoint: '',
      },
    });
    expect(result.success).toBe(false);
  });

  it('does not throw and reports failure for a totally unrelated object', () => {
    expect(() => parseOpenTelemetry({ foo: 'bar' })).not.toThrow();
    expect(parseOpenTelemetry({ foo: 'bar' }).success).toBe(false);
  });

  it('strips unknown extra fields rather than rejecting them (non-strict)', () => {
    const result = parseOpenTelemetry({
      ...validOpenTelemetry,
      some_future_server_field: 'ignored',
    });
    expect(result.success).toBe(true);
    if (result.success) {
      expect(result.data).not.toHaveProperty('some_future_server_field');
    }
  });
});

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
