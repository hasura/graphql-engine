import { removeRequestBodyFromGetOperations } from './ExportOpenAPI';

// We're only testing the removeRequestBodyFromGetOperations function directly
// No need to mock React components or hooks

describe('removeRequestBodyFromGetOperations', () => {
  it('should remove requestBody from GET operations', () => {
    // Create a test OpenAPI spec with a GET operation that has a requestBody
    const testSpec = {
      paths: {
        '/api/test': {
          get: {
            summary: 'Test GET',
            parameters: [
              {
                in: 'query',
                name: 'name',
                schema: {
                  type: 'string',
                },
              },
            ],
            requestBody: {
              content: {
                'application/json': {
                  schema: {
                    properties: {
                      name: {
                        type: 'string',
                      },
                    },
                  },
                },
              },
            },
            responses: {
              '200': {
                description: 'OK',
              },
            },
          },
        },
      },
    };

    // Process the spec
    const processedSpec = removeRequestBodyFromGetOperations(testSpec);

    // Verify that the requestBody was removed from the GET operation
    expect(processedSpec.paths['/api/test'].get.requestBody).toBeUndefined();

    // Verify that other properties were preserved
    expect(processedSpec.paths['/api/test'].get.summary).toBe('Test GET');
    expect(processedSpec.paths['/api/test'].get.parameters).toEqual([
      {
        in: 'query',
        name: 'name',
        schema: {
          type: 'string',
        },
      },
    ]);
  });

  it('should not modify POST operations with requestBody', () => {
    // Create a test OpenAPI spec with a POST operation that has a requestBody
    const testSpec = {
      paths: {
        '/api/test': {
          post: {
            summary: 'Test POST',
            requestBody: {
              content: {
                'application/json': {
                  schema: {
                    properties: {
                      name: {
                        type: 'string',
                      },
                    },
                  },
                },
              },
            },
            responses: {
              '200': {
                description: 'OK',
              },
            },
          },
        },
      },
    };

    // Process the spec
    const processedSpec = removeRequestBodyFromGetOperations(testSpec);

    // Verify that the requestBody was preserved in the POST operation
    expect(processedSpec.paths['/api/test'].post.requestBody).toEqual(
      testSpec.paths['/api/test'].post.requestBody
    );
  });

  it('should handle GET operations without requestBody', () => {
    // Create a test OpenAPI spec with a GET operation that doesn't have a requestBody
    const testSpec = {
      paths: {
        '/api/test': {
          get: {
            summary: 'Test GET',
            parameters: [
              {
                in: 'query',
                name: 'name',
                schema: {
                  type: 'string',
                },
              },
            ],
            responses: {
              '200': {
                description: 'OK',
              },
            },
          },
        },
      },
    };

    // Process the spec
    const processedSpec = removeRequestBodyFromGetOperations(testSpec);

    // Verify that the operation was not modified
    expect(processedSpec.paths['/api/test'].get).toEqual(
      testSpec.paths['/api/test'].get
    );
  });

  it('should handle specs without paths', () => {
    // Create a test OpenAPI spec without paths
    const testSpec = {
      info: {
        title: 'Test API',
        version: '1.0.0',
      },
    };

    // Process the spec
    const processedSpec = removeRequestBodyFromGetOperations(testSpec);

    // Verify that the spec was not modified
    expect(processedSpec).toEqual(testSpec);
  });

  it('should handle empty specs', () => {
    // Create an empty test OpenAPI spec
    const testSpec = {};

    // Process the spec
    const processedSpec = removeRequestBodyFromGetOperations(testSpec);

    // Verify that the spec was not modified
    expect(processedSpec).toEqual(testSpec);
  });
});
