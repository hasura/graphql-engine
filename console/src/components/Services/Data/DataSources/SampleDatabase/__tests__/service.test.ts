import {
  checkNestedFieldValueInErrJson,
  maskPostgresError,
  newSampleDBTrial,
  maskedErrorMessage,
} from '../service';
import { ConsoleType } from '../../../../../../Globals';

describe('Test checkNestedFieldValueInErrJson', () => {
  const testCases = [
    {
      name: 'inexistence in undefined object',
      input: {
        object: undefined,
        key: 'foo',
        value: 'bar',
      },
      output: false,
    },
    {
      name: 'inexistence in empty object',
      input: {
        object: {},
        key: 'foo',
        value: 'bar',
      },
      output: false,
    },
    {
      name: 'inexistence in flat object',
      input: {
        object: { foo: undefined, bar: 'doo' },
        key: 'foo',
        value: 'bar',
      },
      output: false,
    },
    {
      name: 'existence in flat object',
      input: {
        object: { foo: 'bar' },
        key: 'foo',
        value: 'bar',
      },
      output: true,
    },
    {
      name: 'inexistence in nested object',
      input: {
        object: {
          key1: 'value1',
          key2: null,
          key3: { key4: 'value4', key5: 'value5' },
        },
        key: 'foo',
        value: 'bar',
      },
      output: false,
    },
    {
      name: 'existence in nested object',
      input: {
        object: {
          key1: 'value1',
          key2: null,
          key3: { key4: 'value4', key5: undefined, key6: { foo: 'bar' } },
        },
        key: 'foo',
        value: 'bar',
      },
      output: true,
    },
    {
      name: 'inexistence in array of scalars',
      input: {
        object: ['foo', 'bar', 'abcd', false, null, undefined],
        key: 'foo',
        value: 'bar',
      },
      output: false,
    },
    {
      name: 'inexistence in array of objects and scalars',
      input: {
        object: [
          'foo',
          'bar',
          { key1: 'key2' },
          {},
          { key3: null },
          false,
          null,
          undefined,
        ],
        key: 'foo',
        value: 'bar',
      },
      output: false,
    },
    {
      name: 'existence in array of objects and scalars',
      input: {
        object: [
          'foo',
          'bar',
          { key1: 'key2' },
          {},
          { foo: 'bar' },
          false,
          null,
          undefined,
        ],
        key: 'foo',
        value: 'bar',
      },
      output: true,
    },
    {
      name: 'inexistence in array of objects of arrays of objects',
      input: {
        object: [
          'foo',
          'bar',
          { key1: [{ key1: 'key2' }, { key3: 'key4' }] },
          {},
          { key7: 'key8' },
          false,
          null,
          undefined,
        ],
        key: 'foo',
        value: 'bar',
      },
      output: false,
    },
    {
      name: 'existence in array of objects of arrays of objects',
      input: {
        object: [
          'foo',
          'bar',
          {
            key1: [
              { key1: 'key2' },
              { key3: [{ key9: 'key10' }, { foo: 'bar' }] },
            ],
          },
          {},
          { key7: 'key8' },
          false,
          null,
          undefined,
        ],
        key: 'foo',
        value: 'bar',
      },
      output: true,
    },
  ];

  testCases.forEach(tc => {
    test(tc.name, () => {
      expect(
        checkNestedFieldValueInErrJson(
          tc.input.object,
          tc.input.key,
          tc.input.value
        )
      ).toEqual(tc.output);
    });
  });
});

describe('newSampleDBTrial test', () => {
  test('consoleType==oss', () => {
    const sampleDBTrialService = newSampleDBTrial({
      consoleType: 'oss',
      hasuraCloudProjectId: 'project_id',
      cohortConfig: {
        databaseUrl: 'test_db_url',
        status: 'enabled',
      },
    });

    expect(sampleDBTrialService.isActive()).toEqual(false);
    expect(sampleDBTrialService.getDatabaseUrl()).toEqual('');
    expect(sampleDBTrialService.isExploringSampleDB(() => {})).toEqual(false);
  });

  test('consoleType==cloud, null config', () => {
    const sampleDBTrialService = newSampleDBTrial({
      consoleType: 'cloud',
      hasuraCloudProjectId: 'project_id',
      cohortConfig: null,
    });

    expect(sampleDBTrialService.isActive()).toEqual(false);
    expect(sampleDBTrialService.getDatabaseUrl()).toEqual('');
    expect(sampleDBTrialService.isExploringSampleDB(() => {})).toEqual(false);
  });

  test('consoleType==cloud', () => {
    const sampleDBTrialService = newSampleDBTrial({
      consoleType: 'cloud',
      hasuraCloudProjectId: 'project_id',
      cohortConfig: {
        databaseUrl: 'test_db_url',
        status: 'enabled',
      },
    });

    expect(sampleDBTrialService.isActive()).toEqual(true);
    expect(sampleDBTrialService.getDatabaseUrl()).toEqual('test_db_url');
    expect(sampleDBTrialService.isExploringSampleDB(() => {})).toEqual(false);
  });

  test('isExploringSampleDB', () => {
    const sampleDBTrialService = newSampleDBTrial({
      consoleType: 'cloud',
      hasuraCloudProjectId: 'project_id',
      cohortConfig: {
        databaseUrl: 'test_db_url',
        status: 'enabled',
      },
    });

    expect(sampleDBTrialService.isActive()).toEqual(true);
    expect(sampleDBTrialService.getDatabaseUrl()).toEqual('test_db_url');

    // when there are no sources
    expect(
      sampleDBTrialService.isExploringSampleDB(() => ({
        metadata: { metadataObject: { sources: [] } },
      }))
    ).toEqual(false);

    // when there are sources but the sample db source is not added
    expect(
      sampleDBTrialService.isExploringSampleDB(() => ({
        metadata: {
          metadataObject: {
            sources: [
              {
                configuration: {
                  connection_info: { database_url: 'another_db_url' },
                },
              },
            ],
          },
        },
      }))
    ).toEqual(false);

    // when the sample db source is added but other sources are added too
    // assumes that the user is done playing with the sample db
    expect(
      sampleDBTrialService.isExploringSampleDB(() => ({
        metadata: {
          metadataObject: {
            sources: [
              {
                configuration: {
                  connection_info: { database_url: 'another_db_url' },
                },
              },
              {
                configuration: {
                  connection_info: { database_url: 'test_db_url' },
                },
              },
            ],
          },
        },
      }))
    ).toEqual(false);

    // when only the sample db source is added
    // assumes that the user is currently playing with the sample db
    expect(
      sampleDBTrialService.isExploringSampleDB(() => ({
        metadata: {
          metadataObject: {
            sources: [
              {
                configuration: {
                  connection_info: { database_url: 'another_db_url' },
                },
              },
              {
                configuration: {
                  connection_info: { database_url: 'test_db_url' },
                },
              },
            ],
          },
        },
      }))
    ).toEqual(false);

    // when multple ds with the sample db source are added
    // assumes that the user is currently playing with the sample db
    expect(
      sampleDBTrialService.isExploringSampleDB(() => ({
        metadata: {
          metadataObject: {
            sources: [
              {
                configuration: {
                  connection_info: { database_url: 'test_db_url' },
                },
              },
              {
                configuration: {
                  connection_info: { database_url: 'test_db_url' },
                },
              },
            ],
          },
        },
      }))
    ).toEqual(true);
  });

  test('hasAddedSampleDB', () => {
    const sampleDBTrialService = newSampleDBTrial({
      consoleType: 'cloud',
      hasuraCloudProjectId: 'project_id',
      cohortConfig: {
        databaseUrl: 'test_db_url',
        status: 'enabled',
      },
    });
    expect(
      sampleDBTrialService.hasAddedSampleDB(() => ({
        metadata: {
          metadataObject: {
            sources: [
              {
                configuration: {
                  connection_info: { database_url: 'test_db_url' },
                },
              },
            ],
          },
        },
      }))
    ).toEqual(true);
    expect(
      sampleDBTrialService.hasAddedSampleDB(() => ({
        metadata: {
          metadataObject: {
            sources: [
              {
                configuration: {
                  connection_info: { database_url: 'test_db_url' },
                },
              },
              {
                configuration: {
                  connection_info: { database_url: 'test_db_url' },
                },
              },
            ],
          },
        },
      }))
    ).toEqual(true);

    expect(
      sampleDBTrialService.hasAddedSampleDB(() => ({
        metadata: {
          metadataObject: {
            sources: [
              {
                configuration: {
                  connection_info: { database_url: 'test_db_url' },
                },
              },
              {
                configuration: {
                  connection_info: { database_url: 'test_db_url1' },
                },
              },
            ],
          },
        },
      }))
    ).toEqual(true);

    expect(
      sampleDBTrialService.hasAddedSampleDB(() => ({
        metadata: {
          metadataObject: {
            sources: [
              {
                configuration: {
                  connection_info: { database_url: 'test_db_url2' },
                },
              },
              {
                configuration: {
                  connection_info: { database_url: 'test_db_url1' },
                },
              },
            ],
          },
        },
      }))
    ).toEqual(false);

    expect(
      sampleDBTrialService.hasAddedSampleDB(() => ({
        metadata: {
          metadataObject: {
            sources: [],
          },
        },
      }))
    ).toEqual(false);
  });
});

describe('maskPostgresError tests', () => {
  const testCases = [
    {
      name: 'null cohort config, no sources, pg permissions error',
      input: {
        getState: () => ({
          main: {
            cloud: {
              onboardingSampleDB: {
                cohortConfig: null,
              },
            },
            metadata: {
              metadataObject: {
                sources: [],
              },
            },
          },
        }),
        errorJson: {
          status_code: '42501',
        },
        consoleType: 'cloud',
      },
      output: null,
    },
    {
      name: 'valid cohort config, no sources, pg permissions error',
      input: {
        getState: () => ({
          main: {
            cloud: {
              onboardingSampleDB: {
                cohortConfig: {
                  databaseUrl: 'test_db_url',
                  status: 'enabled',
                },
              },
            },
            metadata: {
              metadataObject: {
                sources: [],
              },
            },
          },
        }),
        errorJson: {
          status_code: '42501',
        },
        consoleType: 'cloud',
      },
      output: maskedErrorMessage,
    },
    {
      name: 'no cohort config, connected source, pg permissions error',
      input: {
        getState: () => ({
          main: {
            cloud: {
              onboardingSampleDB: {
                cohortConfig: null,
              },
            },
            metadata: {
              metadataObject: {
                sources: [
                  {
                    configuration: {
                      connection_info: {
                        database_url: 'test_db_url',
                      },
                    },
                  },
                ],
              },
            },
          },
        }),
        errorJson: {
          status_code: '42501',
        },
        consoleType: 'cloud',
      },
      output: null,
    },
    {
      name: 'disabled cohort config, connected source, pg permissions error',
      input: {
        getState: () => ({
          main: {
            cloud: {
              onboardingSampleDB: {
                cohortConfig: {
                  status: 'disabled',
                  databaseUrl: 'test_db_url',
                },
              },
            },
            metadata: {
              metadataObject: {
                sources: [
                  {
                    configuration: {
                      connection_info: {
                        database_url: 'test_db_url',
                      },
                    },
                  },
                ],
              },
            },
          },
        }),
        errorJson: {
          status_code: '42501',
        },
        consoleType: 'cloud',
      },
      output: null,
    },
    {
      name: 'valid cohort config, connected source, pg permissions error',
      input: {
        getState: () => ({
          main: {
            cloud: {
              onboardingSampleDB: {
                cohortConfig: {
                  databaseUrl: 'test_db_url',
                  status: 'enabled',
                },
              },
            },
            metadata: {
              metadataObject: {
                sources: [
                  {
                    configuration: {
                      connection_info: {
                        database_url: 'test_db_url',
                      },
                    },
                  },
                ],
              },
            },
          },
        }),
        errorJson: {
          status_code: '42501',
        },
        consoleType: 'cloud',
      },
      output: maskedErrorMessage,
    },
    {
      name:
        'valid cohort config, connected source, nested pg permissions error',
      input: {
        getState: () => ({
          main: {
            cloud: {
              onboardingSampleDB: {
                cohortConfig: {
                  databaseUrl: 'test_db_url',
                  status: 'enabled',
                },
              },
            },
            metadata: {
              metadataObject: {
                sources: [
                  {
                    configuration: {
                      connection_info: {
                        database_url: 'test_db_url',
                      },
                    },
                  },
                ],
              },
            },
          },
        }),
        errorJson: {
          errors: [
            {
              message: 'errmsg',
              extensions: {
                error: {
                  status_code: '42501',
                },
              },
            },
          ],
        },
        consoleType: 'cloud',
      },
      output: maskedErrorMessage,
    },
    {
      name:
        'valid cohort config, connected source, nested pg permissions error, consoleType !== cloud',
      input: {
        getState: () => ({
          main: {
            cloud: {
              onboardingSampleDB: {
                cohortConfig: {
                  databaseUrl: 'test_db_url',
                  status: 'enabled',
                },
              },
            },
            metadata: {
              metadataObject: {
                sources: [
                  {
                    configuration: {
                      connection_info: {
                        database_url: 'test_db_url',
                      },
                    },
                  },
                ],
              },
            },
          },
        }),
        errorJson: {
          errors: [
            {
              message: 'errmsg',
              extensions: {
                error: {
                  status_code: '42501',
                },
              },
            },
          ],
        },
        consoleType: 'oss',
      },
      output: null,
    },
  ];

  testCases.forEach(tc => {
    test(tc.name, () => {
      expect(
        maskPostgresError(
          tc.input.errorJson,
          tc.input.getState,
          tc.input.consoleType as ConsoleType
        )
      );
    });
  });
});
