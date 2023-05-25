import { Table } from '../../../hasura-metadata-types';
import { runMetadataQuery } from '../../api';
import { GetTrackableTablesProps, IntrospectedFunction } from '../../types';

type TrackableObjects = {
  functions: {
    name: unknown;
    volatility: 'STABLE' | 'VOLATILE';
  }[];
  tables: {
    name: Table;
  }[];
};

export type GetTrackableObjectsResponse = {
  tables: {
    name: string;
    table: Table;
    type: string;
  }[];
  functions: IntrospectedFunction[];
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

export type GetTrackableObjectsProps = Pick<
  GetTrackableTablesProps,
  'httpClient' | 'dataSourceName'
>;

export const getTrackableObjects = async ({
  httpClient,
  dataSourceName,
}: GetTrackableObjectsProps) => {
  try {
    const result = await runMetadataQuery<TrackableObjects>({
      httpClient,
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

    const functions: IntrospectedFunction[] = result.functions.map(fn => {
      return {
        name: adaptName(fn.name),
        qualifiedFunction: fn.name,
        isVolatile: fn.volatility === 'VOLATILE',
      };
    });

    return { tables, functions };
  } catch (error) {
    throw new Error('Error fetching GDC trackable objects');
  }
};
