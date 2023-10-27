export default {
  version: 3,
  sources: [
    {
      name: 'Test Snowflake Hasura',
      kind: 'snowflake',
      tables: [
        {
          table: ['ARTIST'],
          select_permissions: [
            {
              role: 'the_dev',
              permission: { columns: ['NAME'], filter: {} },
            },
            { role: 'the_qa', permission: { columns: [], filter: {} } },
            { role: 'the_user', permission: { columns: [], filter: {} } },
          ],
        },
      ],
      functions: [
        {
          function: ['SEARCH_ALBUMS_BY_TITLE'],
          configuration: {
            custom_root_fields: {},
            response: { table: ['ARTIST'], type: 'table' },
          },
          permissions: [
            { role: 'the_user' },
            { role: 'the_dev' },
            { role: 'the_qa' },
          ],
        },
      ],
      configuration: {
        template: null,
        timeout: null,
        value: {
          fully_qualify_all_names: false,
          jdbc_url:
            'jdbc:snowflake://ak41499.us-east-2.aws.snowflakecomputing.com?user=gdc&password=8yvF5qcRNXA2NPFQ&warehouse=COMPUTE_WH&db=CHINOOK&role=PUBLIC&schema=PUBLIC&CLIENT_SESSION_KEEP_ALIVE=TRUE',
        },
      },
    },
  ],
  remote_schemas: [],
  actions: [],
  custom_types: {
    input_objects: [],
    objects: [],
  },
  cron_triggers: [],
  backend_configs: {
    dataconnector: {
      snowflake: { uri: 'http://data-connector-agent:8081/api/v1/snowflake' },
    },
  },
};
