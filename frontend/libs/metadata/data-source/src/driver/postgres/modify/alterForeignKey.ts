import { PostgresFamilyDriver } from '@hasura/shared/types';
import { ModifyForeignKeyProps } from '../../types';
import { PostgresTable } from '../types';
import { runDatabaseMigration } from '@hasura/metadata/api';
import { getDropConstraintSql } from '../sqlQueries';
import { getTableConstraintDefinitionCurry } from '../introspection';
import { getCreateForeignKeySql } from './createForeignKey';
import { terminateSql } from '../../utils';

export const alterForeignKeyCurry = (kind: PostgresFamilyDriver) => {
  const getTableConstraintDefinition = getTableConstraintDefinitionCurry(kind);

  return async ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    ...args
  }: ModifyForeignKeyProps) => {
    const source = { name: dataSourceName, kind };
    const { name, schema } = args.from.table as PostgresTable;

    const downSql: string[] = [];

    if (isMigration) {
      const oldDefinition = await getTableConstraintDefinition({
        dataSourceName,
        table: args.from.table,
        types: ['f', 'p'],
        constraintName: args.constraintName,
        endpoints,
        fetchJson,
      });

      downSql.push(
        getDropConstraintSql({
          table: args.from.table,
          constraintName: args.constraintName,
        }),
      );

      if (oldDefinition) {
        downSql.push(terminateSql(oldDefinition));
      }
    }

    const upSql = [
      getDropConstraintSql({
        table: args.from.table,
        constraintName: args.constraintName,
      }),
      getCreateForeignKeySql(args),
    ].join('\n');

    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `set_fk_${schema}_${name}_${args.from.columns.join('_')}`,
        up: [
          {
            sql: upSql,
          },
        ],
        down: downSql.length
          ? [
              {
                sql: downSql.join('\n'),
              },
            ]
          : [],
      },
    }).then(() => true);
  };
};
