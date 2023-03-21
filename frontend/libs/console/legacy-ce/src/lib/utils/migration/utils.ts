import { isColumnUnique } from '../../components/Services/Data/TableModify/ModifyActions';
import { quoteDefault } from '../../components/Services/Data/utils';
import gqlPattern from '../../components/Services/Data/Common/GraphQLValidation';
import Migration from './Migration';
import { Table } from '../../dataSources/types';
import {
  dataSource,
  findTable,
  generateTableDef,
  getTableCustomRootFields,
  getTableCustomName,
  getTableColumnConfig,
} from '../../dataSources';
import { getRunSqlQuery } from '../../components/Common/utils/v1QueryUtils';
import { getSetCustomRootFieldsQuery } from '../../metadata/queryUtils';

export { getDownQueryComments } from './getDownQueryComments';
export interface NewColumnType {
  tableName: string;
  type: string;
  isNullable: boolean;
  isUnique: boolean;
  default?: string;
  comment?: string;
  name: string;
  schemaName?: string;
  customFieldName?: string;
}

export interface SchemaType {
  table_name: string;
  table_type: string;
  table_schema: string;
}

export interface OldColumnType {
  data_type: string;
  column_default: string | null;
  comment: string | null;
  is_nullable?: string;
  data_type_name?: string;
}

// utils

const parseNewCol = (newColumn: NewColumnType) => ({
  tableName: newColumn.tableName,
  colType: newColumn.type,
  nullable: newColumn.isNullable,
  unique: newColumn.isUnique,
  colDefault: (newColumn.default || '').trim(),
  comment: (newColumn.comment || '').trim(),
  newName: newColumn.name.trim(),
  currentSchema: newColumn.schemaName,
  customFieldName: (newColumn.customFieldName || '').trim(),
});

const parseOldColumns = (oldColumn: OldColumnType) => ({
  originalColType: oldColumn.data_type_name,
  originalData_type: oldColumn.data_type,
  originalColDefault: oldColumn.column_default || '',
  originalColComment: oldColumn.comment || '',
  originalColNullable: oldColumn.is_nullable,
});

// utility to compare old & new columns and generate up &down migrations
export const getColumnUpdateMigration = (
  oldColumn: OldColumnType,
  newColumn: NewColumnType,
  allSchemas: Table[],
  colName: string,
  onInvalidGqlColName: () => void,
  source: string
) => {
  const {
    tableName,
    colType,
    nullable,
    unique,
    colDefault,
    comment,
    newName,
    currentSchema,
    customFieldName,
  } = parseNewCol(newColumn);

  const tableDef = generateTableDef(tableName, currentSchema);
  const table = findTable(allSchemas, tableDef);
  if (!table || !currentSchema) {
    throw new Error(`Table "${tableDef.name} does not exist`);
  }

  const {
    originalColType,
    originalData_type,
    originalColDefault,
    originalColComment,
    originalColNullable,
  } = parseOldColumns(oldColumn);

  const originalColUnique = isColumnUnique(table, colName);

  const columnChangesUpQuery = dataSource.getAlterColumnTypeSql(
    tableName,
    currentSchema || '',
    colName,
    colType,
    nullable
  );
  const columnChangesDownQuery = dataSource.getAlterColumnTypeSql(
    tableName,
    currentSchema || '',
    colName,
    originalData_type,
    originalColNullable === 'YES'
  );

  const quotedColName = `"${colName}"`;

  const migration = new Migration();

  if (originalColType !== colType) {
    migration.add(
      getRunSqlQuery(columnChangesUpQuery, source),
      getRunSqlQuery(columnChangesDownQuery, source)
    );
  }

  let columnDefaultUpQuery;
  if (colDefault !== '') {
    columnDefaultUpQuery = dataSource.getSetColumnDefaultSql(
      tableName,
      currentSchema,
      colName,
      quoteDefault(colDefault),
      `default_${source}_${currentSchema}_${tableName}_${colName}`
    );
  } else {
    columnDefaultUpQuery = dataSource.getDropColumnDefaultSql(
      tableName,
      currentSchema,
      colName,
      `default_${source}_${currentSchema}_${tableName}_${colName}`
    );
  }

  let columnDefaultDownQuery;
  if (originalColDefault !== '') {
    columnDefaultDownQuery = dataSource.getSetColumnDefaultSql(
      tableName,
      currentSchema,
      colName,
      quoteDefault(originalColDefault),
      `${source}_${tableName}_${colName}_default`
    );
  } else {
    columnDefaultDownQuery = dataSource.getDropColumnDefaultSql(
      tableName,
      currentSchema,
      colName,
      `${source}_${tableName}_${colName}_default`
    );
  }

  // check if default is unchanged and then do a drop. if not skip
  if (originalColDefault !== colDefault) {
    migration.add(
      getRunSqlQuery(columnDefaultUpQuery, source),
      getRunSqlQuery(columnDefaultDownQuery, source)
    );
  }

  /* column nullable up/down migration */
  if (nullable) {
    const nullableUpQuery = dataSource.getDropNotNullSql(
      tableName,
      currentSchema,
      colName,
      colType
    );
    const nullableDownQuery = dataSource.getSetNotNullSql(
      tableName,
      currentSchema,
      colName,
      colType
    );
    if (originalColNullable !== 'YES') {
      migration.add(
        getRunSqlQuery(nullableUpQuery, source),
        getRunSqlQuery(nullableDownQuery, source)
      );
    }
  } else {
    const nullableUpQuery = dataSource.getSetNotNullSql(
      tableName,
      currentSchema,
      colName,
      colType
    );
    const nullableDownQuery = dataSource.getDropNotNullSql(
      tableName,
      currentSchema,
      colName
    );
    if (originalColNullable !== 'NO') {
      migration.add(
        getRunSqlQuery(nullableUpQuery, source),
        getRunSqlQuery(nullableDownQuery, source)
      );
    }
  }

  /* column unique up/down migration */
  if (unique) {
    const uniqueUpQuery = dataSource.getAddUniqueConstraintSql(
      tableName,
      currentSchema,
      `${tableName}_${colName}_key`,
      [quotedColName]
    );
    const uniqueDownQuery = dataSource.getDropConstraintSql(
      tableName,
      currentSchema,
      `${tableName}_${colName}_key`
    );
    // check with original unique
    if (!originalColUnique) {
      migration.add(
        getRunSqlQuery(uniqueUpQuery, source),
        getRunSqlQuery(uniqueDownQuery, source)
      );
    }
  } else {
    const uniqueDownQuery = dataSource.getAddUniqueConstraintSql(
      tableName,
      currentSchema,
      `${tableName}_${colName}_key`,
      [quotedColName]
    );
    const uniqueUpQuery = dataSource.getDropConstraintSql(
      tableName,
      currentSchema,
      `${tableName}_${colName}_key`
    );
    // check with original unique
    if (originalColUnique) {
      migration.add(
        getRunSqlQuery(uniqueUpQuery, source),
        getRunSqlQuery(uniqueDownQuery, source)
      );
    }
  }

  /* column comment up/down migration */
  if (originalColComment !== comment) {
    const columnCommentUpQuery = dataSource.getAlterColumnCommentSql({
      tableName,
      schemaName: currentSchema,
      comment,
      columnName: colName,
      columnType: colType,
    });
    const columnCommentDownQuery = dataSource.getAlterColumnCommentSql({
      tableName,
      schemaName: currentSchema,
      comment: originalColComment,
      columnName: colName,
      columnType: colType,
    });

    migration.add(
      getRunSqlQuery(columnCommentUpQuery, source),
      getRunSqlQuery(columnCommentDownQuery, source)
    );
  }

  /* rename column */
  if (newName && colName !== newName) {
    if (!gqlPattern.test(newName)) {
      onInvalidGqlColName();
    }
    migration.add(
      getRunSqlQuery(
        dataSource.getRenameColumnQuery(
          tableName,
          currentSchema,
          newName,
          colName
        ),
        source
      ),
      getRunSqlQuery(
        dataSource.getRenameColumnQuery(
          tableName,
          currentSchema,
          colName,
          newName
        ),
        source
      )
    );
  }

  const metadataMigration = new Migration();
  /* column custom field up/down migration */
  const existingCustomTableName = getTableCustomName(table);
  const existingColumnConfig = getTableColumnConfig(table);
  const existingRootFields = getTableCustomRootFields(table);
  const newColumnConfig = { ...existingColumnConfig };

  const isCustomFieldNameChanged =
    (customFieldName || null) !==
    (existingColumnConfig[colName]?.custom_name || null);
  if (isCustomFieldNameChanged) {
    const columnConfigValue = existingColumnConfig[colName]
      ? { ...existingColumnConfig[colName] }
      : {};
    columnConfigValue.custom_name = customFieldName || null;
    delete newColumnConfig[colName];
    newColumnConfig[newName || colName] = columnConfigValue;

    metadataMigration.add(
      getSetCustomRootFieldsQuery(
        tableDef,
        existingRootFields,
        newColumnConfig,
        existingCustomTableName,
        source
      ),
      getSetCustomRootFieldsQuery(
        tableDef,
        existingRootFields,
        existingColumnConfig,
        existingCustomTableName,
        source
      )
    );
  }

  const migrationName = `alter_table_${currentSchema}_${tableName}_alter_column_${colName}`;
  return {
    migrationName,
    migration,
    metadataMigration: {
      migration: metadataMigration,
      migrationName: `alter_table_${currentSchema}_${tableName}_alter_column_${colName}_custom_fields`,
    },
  };
};
