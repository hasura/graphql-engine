import { getQualifiedTable } from '../../../../features/Data/ManageTable/utils';
import { DriverInfo } from '../../../../features/DataSource';
import {
  InconsistentMetadata,
  InconsistentObject,
} from '../../../../features/hasura-metadata-api';
import { Metadata } from '../../../../features/hasura-metadata-types';
import { DataSourceNode } from './types';

type InconsistentData = {
  inconsistentSources: InconsistentObject[];
  inconsistentTables: InconsistentObject[];
  inconsistentFunctions: InconsistentObject[];
};

export const adaptInconsistentObjects = (m: InconsistentMetadata) =>
  m.inconsistent_objects.reduce<InconsistentData>(
    (acc, entry) => {
      if (entry.type === 'source') acc.inconsistentSources.push(entry);

      if (entry.type === 'table') acc.inconsistentTables.push(entry);

      if (entry.type === 'function') acc.inconsistentFunctions.push(entry);

      return acc;
    },
    {
      inconsistentSources: [],
      inconsistentTables: [],
      inconsistentFunctions: [],
    }
  );

export const adaptSourcesIntoTreeData =
  (m: Metadata) =>
  (drivers: DriverInfo[], inconsistentData: InconsistentData) =>
    m.metadata.sources.map<DataSourceNode>(source => {
      return {
        id: JSON.stringify({ dataSourceName: source.name }),
        dataSourceName: source.name,
        name: source.name,
        driver: source.kind,
        releaseType: drivers?.find(driver => source.kind === driver.name)
          ?.release,
        inconsistentObject: inconsistentData.inconsistentSources.find(
          i => i.definition === source.name
        ),
        children: [
          ...source.tables.map(t => ({
            id: JSON.stringify({
              dataSourceName: source.name,
              table: t.table,
            }),
            table: t.table,
            name: getQualifiedTable(t.table).join(' / '),
          })),
          ...(source.functions ?? []).map(f => ({
            id: JSON.stringify({
              dataSourceName: source.name,
              function: f.function,
            }),
            function: f.function,
            name: getQualifiedTable(f.function).join(' / '),
          })),
        ],
      };
    });
