import { getMutationRoot } from './getMutationRoot';

describe('getMutationRoot', () => {
  test.each`
    driver        | table                                  | operation         | defaultQueryRoot | tableConfiguration                                                                        | sourceCustomization | output
    ${'postgres'} | ${{ name: 'Album', schema: 'public' }} | ${'delete'}       | ${'Album'}       | ${undefined}                                                                              | ${undefined}        | ${'delete_Album'}
    ${'postgres'} | ${{ name: 'Album', schema: 'public' }} | ${'delete_by_pk'} | ${'Album'}       | ${undefined}                                                                              | ${undefined}        | ${'delete_Album_by_pk'}
    ${'postgres'} | ${{ name: 'Album', schema: 'public' }} | ${'update'}       | ${'Album'}       | ${undefined}                                                                              | ${undefined}        | ${'update_Album'}
    ${'postgres'} | ${{ name: 'Album', schema: 'public' }} | ${'update_by_pk'} | ${'Album'}       | ${undefined}                                                                              | ${undefined}        | ${'update_Album_by_pk'}
    ${'postgres'} | ${{ name: 'Album', schema: 'public' }} | ${'insert'}       | ${'Album'}       | ${undefined}                                                                              | ${undefined}        | ${'insert_Album'}
    ${'postgres'} | ${{ name: 'Album', schema: 'public' }} | ${'insert_one'}   | ${'Album'}       | ${undefined}                                                                              | ${undefined}        | ${'insert_Album_one'}
    ${'postgres'} | ${{ name: 'Album', schema: 'public' }} | ${'delete'}       | ${'Album'}       | ${{ custom_name: 'CustomTableName' }}                                                     | ${undefined}        | ${'delete_CustomTableName'}
    ${'postgres'} | ${{ name: 'Album', schema: 'public' }} | ${'delete'}       | ${'Album'}       | ${{ custom_root_fields: { delete: 'CustomDeleteRoot' } }}                                 | ${undefined}        | ${'CustomDeleteRoot'}
    ${'postgres'} | ${{ name: 'Album', schema: 'public' }} | ${'delete'}       | ${'Album'}       | ${{ custom_name: 'CustomTableName', custom_root_fields: { delete: 'CustomDeleteRoot' } }} | ${undefined}        | ${'CustomDeleteRoot'}
    ${'postgres'} | ${{ name: 'Album', schema: 'public' }} | ${'update'}       | ${'Album'}       | ${{ custom_name: 'CustomTableName' }}                                                     | ${undefined}        | ${'update_CustomTableName'}
    ${'postgres'} | ${{ name: 'Album', schema: 'public' }} | ${'update'}       | ${'Album'}       | ${{ custom_root_fields: { update: 'CustomUpdateRoot' } }}                                 | ${undefined}        | ${'CustomUpdateRoot'}
    ${'postgres'} | ${{ name: 'Album', schema: 'public' }} | ${'update'}       | ${'Album'}       | ${{ custom_name: 'CustomTableName', custom_root_fields: { update: 'CustomUpdateRoot' } }} | ${undefined}        | ${'CustomUpdateRoot'}
    ${'postgres'} | ${{ name: 'Album', schema: 'public' }} | ${'insert'}       | ${'Album'}       | ${{ custom_name: 'CustomTableName' }}                                                     | ${undefined}        | ${'insert_CustomTableName'}
    ${'postgres'} | ${{ name: 'Album', schema: 'public' }} | ${'insert'}       | ${'Album'}       | ${{ custom_root_fields: { insert: 'CustomInsertRoot' } }}                                 | ${undefined}        | ${'CustomInsertRoot'}
    ${'postgres'} | ${{ name: 'Album', schema: 'public' }} | ${'insert'}       | ${'Album'}       | ${{ custom_name: 'CustomTableName', custom_root_fields: { insert: 'CustomInsertRoot' } }} | ${undefined}        | ${'CustomInsertRoot'}
  `(
    `$driver $table $operation $defaultQueryRoot $tableConfiguration $sourceCustomization $output`,
    ({
      operation,
      defaultQueryRoot,
      tableConfiguration: configuration,
      sourceCustomization,
      output,
    }) => {
      const result = getMutationRoot({
        operation,
        defaultQueryRoot,
        configuration,
        sourceCustomization,
      });
      expect(result).toBe(output);
    }
  );
});
