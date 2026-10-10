import { Table, TableFunction } from '@hasura/shared/types';
import { GetTrackableObjectsProps, IntrospectedFunction } from '../../types';
import { runMetadataQuery } from '@hasura/metadata/api';

type TrackableObjects = {
  functions: {
    name: TableFunction;
    volatility: 'STABLE' | 'VOLATILE';
  }[];
  tables: {
    name: Table;
  }[];
};

const adaptName = (name: unknown): string => {
  if (typeof name === 'string') {
    return name;
  }
  if (Array.isArray(name)) {
    return name.join('.');
  }

  throw Error('getTrackableObjects: name is not string nor array:' + name);
};

export const getTrackableObjects = async ({
  endpoints,
  fetchJson,
  dataSourceName,
}: GetTrackableObjectsProps) => {
  try {
    const result = await runMetadataQuery<TrackableObjects>({
      url: endpoints.metadata,
      fetchJson,
      body: {
        type: 'reference_get_source_trackables',
        args: {
          source: dataSourceName,
        },
      },
    });

    const tables = result.tables.map(({ name }) => {
      /**
       * Ideally each table is supposed to be GDCTable, but the server fix has not yet been merged to main.
       * Right now it returns string as a table.
       */
      return {
        name: adaptName(name),
        table: name,
        type: 'BASE TABLE',
      };
    });

    const functions: IntrospectedFunction[] = result.functions.map((fn) => {
      return {
        name: adaptName(fn.name),
        function: fn.name,
        isVolatile: fn.volatility === 'VOLATILE',
      };
    });

    return { tables, functions };
  } catch (error) {
    throw new Error('Error fetching GDC trackable objects');
  }
};
