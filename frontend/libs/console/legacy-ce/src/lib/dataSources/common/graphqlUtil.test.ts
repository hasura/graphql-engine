import { getQueryWithNamespace, getFullQueryNameBase } from './graphqlUtil';
import { formatSdl } from 'format-graphql';

describe('getQueryWithNamespace', () => {
  it('returns a query with namespace', () => {
    const result = getQueryWithNamespace({
      namespace: 'namespace',
      queryName: 'query Rows',
      innerQuery: 'Album { id }',
    });
    const expected = formatSdl(`query Rows
    {
      namespace {
        Album { id }
      }
    }`);
    expect(result).toEqual(expected);
  });

  it('returns a query without namespace', () => {
    const result = getQueryWithNamespace({
      namespace: '',
      queryName: 'query Rows',
      innerQuery: 'Album { id }',
    });

    const expected = formatSdl(`query Rows
    {
      Album { id }
    }`);

    expect(result).toEqual(expected);
  });
});

describe('getFullQueryNameBase', () => {
  it.each`
    custom_name | custom_root_fields                             | namespace | prefix    | suffix    | operation             | expected
    ${''}       | ${{}}                                          | ${''}     | ${''}     | ${''}     | ${'select'}           | ${'Album'}
    ${'alb'}    | ${{}}                                          | ${''}     | ${''}     | ${''}     | ${'select'}           | ${'alb'}
    ${''}       | ${{}}                                          | ${'ns'}   | ${''}     | ${''}     | ${'select'}           | ${'Album'}
    ${''}       | ${{}}                                          | ${''}     | ${'pre_'} | ${''}     | ${'select'}           | ${'pre_Album'}
    ${''}       | ${{}}                                          | ${''}     | ${''}     | ${'_suf'} | ${'select'}           | ${'Album_suf'}
    ${''}       | ${{}}                                          | ${'ns'}   | ${'pre_'} | ${'_suf'} | ${'select'}           | ${'pre_Album_suf'}
    ${''}       | ${{ select: 'my_select' }}                     | ${'ns'}   | ${'pre_'} | ${'_suf'} | ${'select'}           | ${'pre_my_select_suf'}
    ${'alb'}    | ${{ select: 'my_select' }}                     | ${'ns'}   | ${'pre_'} | ${'_suf'} | ${'select'}           | ${'pre_my_select_suf'}
    ${''}       | ${{}}                                          | ${''}     | ${''}     | ${''}     | ${'update'}           | ${'update_Album'}
    ${'alb'}    | ${{}}                                          | ${''}     | ${''}     | ${''}     | ${'update'}           | ${'update_alb'}
    ${''}       | ${{}}                                          | ${'ns'}   | ${''}     | ${''}     | ${'update'}           | ${'update_Album'}
    ${''}       | ${{}}                                          | ${''}     | ${'pre_'} | ${''}     | ${'update'}           | ${'pre_update_Album'}
    ${''}       | ${{}}                                          | ${''}     | ${''}     | ${'_suf'} | ${'update'}           | ${'update_Album_suf'}
    ${''}       | ${{}}                                          | ${'ns'}   | ${'pre_'} | ${'_suf'} | ${'update'}           | ${'pre_update_Album_suf'}
    ${''}       | ${{ update: 'my_update' }}                     | ${'ns'}   | ${'pre_'} | ${'_suf'} | ${'update'}           | ${'pre_my_update_suf'}
    ${'alb'}    | ${{ update: 'my_update' }}                     | ${'ns'}   | ${'pre_'} | ${'_suf'} | ${'select'}           | ${'pre_alb_suf'}
    ${''}       | ${{}}                                          | ${''}     | ${''}     | ${''}     | ${'select_aggregate'} | ${'Album_aggregate'}
    ${'alb'}    | ${{}}                                          | ${''}     | ${''}     | ${''}     | ${'select_aggregate'} | ${'alb_aggregate'}
    ${''}       | ${{}}                                          | ${'ns'}   | ${''}     | ${''}     | ${'select_aggregate'} | ${'Album_aggregate'}
    ${''}       | ${{}}                                          | ${''}     | ${'pre_'} | ${''}     | ${'select_aggregate'} | ${'pre_Album_aggregate'}
    ${''}       | ${{}}                                          | ${''}     | ${''}     | ${'_suf'} | ${'select_aggregate'} | ${'Album_aggregate_suf'}
    ${''}       | ${{}}                                          | ${'ns'}   | ${'pre_'} | ${'_suf'} | ${'select_aggregate'} | ${'pre_Album_aggregate_suf'}
    ${''}       | ${{ select_aggregate: 'my_select_aggregate' }} | ${'ns'}   | ${'pre_'} | ${'_suf'} | ${'select_aggregate'} | ${'pre_my_select_aggregate_suf'}
    ${'alb'}    | ${{ select_aggregate: 'my_select_aggregate' }} | ${'ns'}   | ${'pre_'} | ${'_suf'} | ${'select_aggregate'} | ${'pre_my_select_aggregate_suf'}
    ${''}       | ${{}}                                          | ${''}     | ${''}     | ${''}     | ${'insert'}           | ${'insert_Album'}
    ${'alb'}    | ${{}}                                          | ${''}     | ${''}     | ${''}     | ${'insert'}           | ${'insert_alb'}
    ${''}       | ${{}}                                          | ${'ns'}   | ${''}     | ${''}     | ${'insert'}           | ${'insert_Album'}
    ${''}       | ${{}}                                          | ${''}     | ${'pre_'} | ${''}     | ${'insert'}           | ${'pre_insert_Album'}
    ${''}       | ${{}}                                          | ${''}     | ${''}     | ${'_suf'} | ${'insert'}           | ${'insert_Album_suf'}
    ${''}       | ${{}}                                          | ${'ns'}   | ${'pre_'} | ${'_suf'} | ${'insert'}           | ${'pre_insert_Album_suf'}
    ${''}       | ${{ insert: 'my_insert' }}                     | ${''}     | ${''}     | ${''}     | ${'insert'}           | ${'my_insert'}
    ${'alb'}    | ${{ insert: 'my_insert' }}                     | ${'ns'}   | ${'pre_'} | ${'_suf'} | ${'insert'}           | ${'pre_my_insert_suf'}
    ${''}       | ${{}}                                          | ${''}     | ${''}     | ${''}     | ${'delete'}           | ${'delete_Album'}
    ${'alb'}    | ${{}}                                          | ${''}     | ${''}     | ${''}     | ${'delete'}           | ${'delete_alb'}
    ${''}       | ${{}}                                          | ${'ns'}   | ${''}     | ${''}     | ${'delete'}           | ${'delete_Album'}
    ${''}       | ${{}}                                          | ${''}     | ${'pre_'} | ${''}     | ${'delete'}           | ${'pre_delete_Album'}
    ${''}       | ${{}}                                          | ${''}     | ${''}     | ${'_suf'} | ${'delete'}           | ${'delete_Album_suf'}
    ${''}       | ${{}}                                          | ${'ns'}   | ${'pre_'} | ${'_suf'} | ${'delete'}           | ${'pre_delete_Album_suf'}
    ${''}       | ${{ delete: 'my_delete' }}                     | ${''}     | ${''}     | ${''}     | ${'delete'}           | ${'my_delete'}
    ${'alb'}    | ${{ delete: 'my_delete' }}                     | ${''}     | ${''}     | ${''}     | ${'delete'}           | ${'my_delete'}
  `(
    'given custom name: "$custom_name", custom_root_fields: $custom_root_fields, namespace: "$namespace", prefix: "$prefix", suffix: "$suffix", operation: "$operation", returns "$expected"',
    ({
      custom_name,
      custom_root_fields,
      namespace,
      prefix,
      suffix,
      operation,
      expected,
    }) => {
      const result = getFullQueryNameBase('public')({
        tableName: 'Album',
        schema: 'public',
        tableConfiguration: {
          custom_name,
          custom_root_fields,
        },
        dataSourceCustomization: {
          root_fields: {
            namespace,
            prefix,
            suffix,
          },
        },
        operation,
      });

      expect(result).toEqual(expected);
    }
  );
});
