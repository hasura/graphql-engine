import {
  getDataTriggerLogsCountQuery,
  getDataTriggerLogsQuery,
  getDataTriggerInvocations,
} from '../metadataTableUtils';

describe('MetadatTable_Utils_getDataTriggerLogsCountQuery()', () => {
  it('should generate SQL query for pending event count ', () => {
    const pendingCountQuery = getDataTriggerLogsCountQuery(
      'new_user',
      'pending',
      'default'
    );
    expect(pendingCountQuery?.args?.sql).toBeTruthy();
    expect(pendingCountQuery?.args?.source).toEqual('default');
    expect(pendingCountQuery?.args?.sql).toContain(
      'delivered=false AND error=false AND archived=false'
    );
    expect(pendingCountQuery?.args?.sql).toContain(
      "data_table.trigger_name = 'new_user'"
    );
    expect(pendingCountQuery?.args?.sql).toContain(
      'FROM "hdb_catalog"."event_log"'
    );
    expect(pendingCountQuery).toMatchSnapshot();
  });

  it('should generate SQL query for processed event count', () => {
    const processedCountQuery = getDataTriggerLogsCountQuery(
      'new_user',
      'processed',
      'db2'
    );
    expect(processedCountQuery?.args?.sql).toBeTruthy();
    expect(processedCountQuery?.args?.sql).toContain(
      'AND (delivered=true OR error=true) AND archived=false'
    );
    expect(processedCountQuery?.args?.source).toEqual('db2');

    expect(processedCountQuery?.args?.sql).toContain(
      "data_table.trigger_name = 'new_user'"
    );
    expect(processedCountQuery?.args?.sql).toContain(
      'FROM "hdb_catalog"."event_log"'
    );

    expect(processedCountQuery).toMatchSnapshot();
  });

  it('should generate SQL query for invocation event count', () => {
    const invocationCountQuery = getDataTriggerLogsCountQuery(
      'test_event',
      'invocation',
      'default'
    );
    expect(invocationCountQuery?.args?.sql).toBeTruthy();
    expect(invocationCountQuery?.args?.sql).toContain(
      "data_table.trigger_name = 'test_event'"
    );
    expect(invocationCountQuery?.args?.source).toEqual('default');

    expect(invocationCountQuery?.args?.sql).toContain(
      'FROM "hdb_catalog"."event_invocation_logs"'
    );
    expect(invocationCountQuery).toMatchSnapshot();
  });
});
describe('MetadatTable_Utils_getDataTriggerLogsQuery()', () => {
  it('should generate SQL query for pending event logs', () => {
    // pending
    const pendingLogsQuery = getDataTriggerLogsQuery(
      'pending',
      'default',
      'new_user',
      10,
      10
    );
    expect(pendingLogsQuery?.args?.sql).toBeTruthy();
    expect(pendingLogsQuery?.args?.sql).toContain(
      'delivered=false AND error=false AND archived=false'
    );
    expect(pendingLogsQuery?.args?.source).toEqual('default');

    expect(pendingLogsQuery?.args?.sql).toContain(
      "data_table.trigger_name = 'new_user'"
    );
    expect(pendingLogsQuery?.args?.sql).toContain(
      'FROM "hdb_catalog"."event_log"'
    );
    expect(pendingLogsQuery?.args?.sql).toContain('LIMIT 10');
    expect(pendingLogsQuery?.args?.sql).toContain('OFFSET 10');
    expect(pendingLogsQuery).toMatchSnapshot();
  });
  it('should generate SQL query for processed event logs', () => {
    // Processed
    const processedLogsQuery = getDataTriggerLogsQuery(
      'processed',
      'db2',
      'test_event',
      100,
      0
    );
    expect(processedLogsQuery?.args?.sql).toBeTruthy();
    expect(processedLogsQuery?.args?.source).toEqual('db2');

    expect(processedLogsQuery?.args?.sql).toContain(
      'AND (delivered=true OR error=true) AND archived=false'
    );
    expect(processedLogsQuery?.args?.sql).toContain(
      "data_table.trigger_name = 'test_event'"
    );
    expect(processedLogsQuery?.args?.sql).toContain(
      'FROM "hdb_catalog"."event_log"'
    );
    expect(processedLogsQuery?.args?.sql).toContain('LIMIT 100');
    expect(processedLogsQuery?.args?.sql).toContain('OFFSET 0');
    expect(processedLogsQuery).toMatchSnapshot();
  });
  it('should generate SQL query for event invocation logs', () => {
    // Invocation
    const invocationLogsQuery = getDataTriggerLogsQuery(
      'processed',
      'db2',
      'test_event',
      100,
      0
    );
    expect(invocationLogsQuery?.args?.sql).toBeTruthy();
    expect(invocationLogsQuery?.args?.source).toEqual('db2');

    expect(invocationLogsQuery?.args?.sql).toContain(
      'AND (delivered=true OR error=true) AND archived=false'
    );
    expect(invocationLogsQuery?.args?.sql).toContain(
      "data_table.trigger_name = 'test_event'"
    );
    expect(invocationLogsQuery?.args?.sql).toContain(
      'FROM "hdb_catalog"."event_log"'
    );
    expect(invocationLogsQuery?.args?.sql).toContain('LIMIT 100');
    expect(invocationLogsQuery?.args?.sql).toContain('OFFSET 0');
    expect(invocationLogsQuery).toMatchSnapshot();
  });
});
describe('MetadatTable_Utils_getDataTriggerInvocations()', () => {
  it('should generate SQL to fetch invocations for an event', () => {
    // pending
    const pendingLogsQuery = getDataTriggerInvocations(
      '298f6a71-f503-46f1-814c-45daef0afe4d',
      'db2'
    );
    expect(pendingLogsQuery?.args?.sql).toBeTruthy();
    expect(pendingLogsQuery?.args?.sql).toContain(
      "event_id = '298f6a71-f503-46f1-814c-45daef0afe4d'"
    );
    expect(pendingLogsQuery?.args?.source).toEqual('db2');

    expect(pendingLogsQuery?.args?.sql).toContain('created_at DESC NULLS LAST');
    expect(pendingLogsQuery?.args?.sql).toContain(
      `FROM 
      "hdb_catalog"."event_invocation_logs"`
    );
    expect(pendingLogsQuery).toMatchSnapshot();
  });
  it('should generate SQL to fetch invocations for an event with default source', () => {
    // pending
    const pendingLogsQuery = getDataTriggerInvocations(
      '298f6a71-f503-46f1-814c-45daef0afe4d'
    );
    expect(pendingLogsQuery?.args?.sql).toBeTruthy();
    expect(pendingLogsQuery?.args?.sql).toContain(
      "event_id = '298f6a71-f503-46f1-814c-45daef0afe4d'"
    );
    expect(pendingLogsQuery?.args?.source).toEqual('default');
  });
});
