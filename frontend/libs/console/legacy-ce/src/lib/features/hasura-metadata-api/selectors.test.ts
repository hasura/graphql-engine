import * as MetadataSelectors from './selectors';
import {
  metadataSource,
  metadataTables,
  metadataWithoutSources,
  metadataWithResourceVersion,
  metadataWithSourcesAndTables,
} from './mocks/metadata.mock';

describe('MetadataSelectors.getSources', () => {
  test.each`
    case                         | metadata                                                          | expectedOutput
    ${' Metadata has no source'} | ${{ resource_version: 1, metadata: { version: 3, sources: [] } }} | ${[]}
    ${' Metadata has sources'}   | ${metadataWithSourcesAndTables}                                   | ${[metadataSource]}
    ${' Metadata is undefined'}  | ${undefined}                                                      | ${undefined}
  `(
    'when invoked for $case, should return $expectedOutput',
    ({ metadata, expectedOutput }) => {
      const result = MetadataSelectors.getSources()(metadata);
      expect(result).toEqual(expectedOutput);
    }
  );
});

describe('MetadataSelectors.findSource', () => {
  test.each`
    case                                    | dataSourceName        | metadata                                                          | expectedOutput
    ${' Metadata has no source'}            | ${'sqlite_test'}      | ${{ resource_version: 1, metadata: { version: 3, sources: [] } }} | ${undefined}
    ${' Metadata has the source'}           | ${'sqlite_test'}      | ${metadataWithSourcesAndTables}                                   | ${metadataSource}
    ${' Metadata does not have the source'} | ${'some_source_name'} | ${metadataWithSourcesAndTables}                                   | ${undefined}
    ${' Metadata is undefined'}             | ${'sqlite_test'}      | ${undefined}                                                      | ${undefined}
  `(
    'when invoked for $case, should return $expectedOutput',
    ({ dataSourceName, metadata, expectedOutput }) => {
      const result = MetadataSelectors.findSource(dataSourceName)(metadata);
      expect(result).toEqual(expectedOutput);
    }
  );
});

describe('MetadataSelectors.findTable', () => {
  test.each`
    case                                            | dataSourceName   | table          | metadata                                                          | expectedOutput
    ${' Metadata has no sources'}                   | ${'sqlite_test'} | ${['Album']}   | ${{ resource_version: 1, metadata: { version: 3, sources: [] } }} | ${undefined}
    ${' Metadata has the source and the table'}     | ${'sqlite_test'} | ${['Album']}   | ${metadataWithSourcesAndTables}                                   | ${{ table: ['Album'] }}
    ${' Metadata has the source but not the table'} | ${'sqlite_test'} | ${['Invoice']} | ${metadataWithSourcesAndTables}                                   | ${undefined}
    ${' Metadata is undefined'}                     | ${'sqlite_test'} | ${['Album']}   | ${undefined}                                                      | ${undefined}
  `(
    'when invoked for $case, should return $expectedOutput',
    ({ dataSourceName, table, metadata, expectedOutput }) => {
      const result = MetadataSelectors.findTable(
        dataSourceName,
        table
      )(metadata);
      expect(result).toEqual(expectedOutput);
    }
  );
});

describe('MetadataSelectors.getTables', () => {
  test.each`
    case                                    | dataSourceName        | metadata                        | expectedOutput
    ${' Metadata has no source'}            | ${'sqlite_test'}      | ${metadataWithoutSources}       | ${undefined}
    ${' Metadata has the source'}           | ${'sqlite_test'}      | ${metadataWithSourcesAndTables} | ${metadataTables}
    ${' Metadata does not have the source'} | ${'some_source_name'} | ${metadataWithSourcesAndTables} | ${undefined}
    ${' Metadata is undefined'}             | ${'sqlite_test'}      | ${undefined}                    | ${undefined}
  `(
    'when invoked for $case, should return $expectedOutput',
    ({ dataSourceName, metadata, expectedOutput }) => {
      const result = MetadataSelectors.getTables(dataSourceName)(metadata);
      expect(result).toEqual(expectedOutput);
    }
  );
});

describe('MetadataSelectors.resourceVersion', () => {
  test.each`
    case                                               | metadata                        | expectedOutput
    ${' Metadata is missing the resource_version key'} | ${metadataWithResourceVersion}  | ${undefined}
    ${' Metadata is undefined'}                        | ${undefined}                    | ${undefined}
    ${' Metadata has the resource version'}            | ${metadataWithSourcesAndTables} | ${73}
  `(
    'when invoked for $case, should return $expectedOutput',
    ({ metadata, expectedOutput }) => {
      const result = MetadataSelectors.resourceVersion()(metadata);
      expect(result).toEqual(expectedOutput);
    }
  );
});
