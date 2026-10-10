import { runSQL } from '@hasura/metadata/api';
import { PostgresFamilyDriver } from '@hasura/shared/types';
import { GetCheckConstraintsProps, TableCheckConstraint } from '../../types';
import { PostgresTable } from '../types';
import { checkConstraintsSql } from '../sqlQueries';
import { adaptCheckConstraints } from './adaptCheckConstraints';

export const getCheckConstraintsCurry =
  (kind: PostgresFamilyDriver) =>
  async ({
    dataSourceName,
    table,
    fetchJson,
    endpoints,
  }: GetCheckConstraintsProps): Promise<TableCheckConstraint[]> => {
    const { schema, name } = table as PostgresTable;

    const sql = checkConstraintsSql({
      tables: [{ name, schema }],
    });

    const response = await runSQL({
      args: {
        source: { name: dataSourceName, kind },
        sql,
        readOnly: true,
      },
      fetchJson,
      url: endpoints.queryV2,
    });

    return adaptCheckConstraints(response.result);
  };

export const getCheckConstraints = getCheckConstraintsCurry('postgres');
