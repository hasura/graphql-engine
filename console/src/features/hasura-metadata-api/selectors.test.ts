import { MetadataSelector } from './selectors';
import {
  metadataSource,
  metadataTables,
  metadataWithoutSources,
  metadataWithResourceVersion,
  metadataWithSourcesAndTables,
} from './mocks/metadata.mock';

describe('MetadataSelector.getMetadataSources', () => {
  test.each`
    case                         | metadata                                                          | expectedOutput
    ${' Metadata has no source'} | ${{ resource_version: 1, metadata: { version: 3, sources: [] } }} | ${[]}
    ${' Metadata has sources'}   | ${metadataWithSourcesAndTables}                                   | ${[metadataSource]}
    ${' Metadata is undefined'}  | ${undefined}                                                      | ${undefined}
  `(
    'when invoked for $case, should return $expectedOutput',
    ({ metadata, expectedOutput }) => {
      const result = MetadataSelector.getMetadataSources()(metadata);
      expect(result).toEqual(expectedOutput);
    }
  );
});

describe('MetadataSelector.findMetadataSource', () => {
  test.each`
    case                                    | dataSourceName        | metadata                                                          | expectedOutput
    ${' Metadata has no source'}            | ${'sqlite_test'}      | ${{ resource_version: 1, metadata: { version: 3, sources: [] } }} | ${undefined}
    ${' Metadata has the source'}           | ${'sqlite_test'}      | ${metadataWithSourcesAndTables}                                   | ${metadataSource}
    ${' Metadata does not have the source'} | ${'some_source_name'} | ${metadataWithSourcesAndTables}                                   | ${undefined}
    ${' Metadata is undefined'}             | ${'sqlite_test'}      | ${undefined}                                                      | ${undefined}
  `(
    'when invoked for $case, should return $expectedOutput',
    ({ dataSourceName, metadata, expectedOutput }) => {
      const result =
        MetadataSelector.findMetadataSource(dataSourceName)(metadata);
      expect(result).toEqual(expectedOutput);
    }
  );
});

describe('MetadataSelector.findMetadataTable', () => {
  test.each`
    case                                            | dataSourceName   | table          | metadata                                                          | expectedOutput
    ${' Metadata has no sources'}                   | ${'sqlite_test'} | ${['Album']}   | ${{ resource_version: 1, metadata: { version: 3, sources: [] } }} | ${undefined}
    ${' Metadata has the source and the table'}     | ${'sqlite_test'} | ${['Album']}   | ${metadataWithSourcesAndTables}                                   | ${{ table: ['Album'] }}
    ${' Metadata has the source but not the table'} | ${'sqlite_test'} | ${['Invoice']} | ${metadataWithSourcesAndTables}                                   | ${undefined}
    ${' Metadata is undefined'}                     | ${'sqlite_test'} | ${['Album']}   | ${undefined}                                                      | ${undefined}
  `(
    'when invoked for $case, should return $expectedOutput',
    ({ dataSourceName, table, metadata, expectedOutput }) => {
      const result = MetadataSelector.findMetadataTable(
        dataSourceName,
        table
      )(metadata);
      expect(result).toEqual(expectedOutput);
    }
  );
});

describe('MetadataSelector.getMetadataTables', () => {
  test.each`
    case                                    | dataSourceName        | metadata                        | expectedOutput
    ${' Metadata has no source'}            | ${'sqlite_test'}      | ${metadataWithoutSources}       | ${undefined}
    ${' Metadata has the source'}           | ${'sqlite_test'}      | ${metadataWithSourcesAndTables} | ${metadataTables}
    ${' Metadata does not have the source'} | ${'some_source_name'} | ${metadataWithSourcesAndTables} | ${undefined}
    ${' Metadata is undefined'}             | ${'sqlite_test'}      | ${undefined}                    | ${undefined}
  `(
    'when invoked for $case, should return $expectedOutput',
    ({ dataSourceName, metadata, expectedOutput }) => {
      const result =
        MetadataSelector.getMetadataTables(dataSourceName)(metadata);
      expect(result).toEqual(expectedOutput);
    }
  );
});

describe('MetadataSelector.getMetadataResourceVersion', () => {
  test.each`
    case                                               | metadata                        | expectedOutput
    ${' Metadata is missing the resource_version key'} | ${metadataWithResourceVersion}  | ${undefined}
    ${' Metadata is undefined'}                        | ${undefined}                    | ${undefined}
    ${' Metadata has the resource version'}            | ${metadataWithSourcesAndTables} | ${73}
  `(
    'when invoked for $case, should return $expectedOutput',
    ({ metadata, expectedOutput }) => {
      const result = MetadataSelector.getMetadataResourceVersion()(metadata);
      expect(result).toEqual(expectedOutput);
    }
  );
});
