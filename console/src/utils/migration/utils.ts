import {
  generateTableDef,
  findTable,
  getTableCustomColumnNames,
  getTableCustomRootFields,
  Table,
} from '../../components/Common/utils/pgUtils';
import { isColumnUnique } from '../../components/Services/Data/TableModify/ModifyActions';
import {
  getRunSqlQuery,
  getSetCustomRootFieldsQuery,
  CustomRootFields,
} from '../../components/Common/utils/v1QueryUtils';
import {
  isColTypeString,
  isPostgresFunction,
  isTypeCast,
} from '../../components/Services/Data/utils';
import gqlPattern from '../../components/Services/Data/Common/GraphQLValidation';
import { sqlEscapeText } from '../../components/Common/utils/sqlUtils';
import Migration from './Migration';
import { RunSqlType } from '../../types';

// Types
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
  udt_name: string;
  data_type: string;
  column_default: string | null;
  comment: string | null;
  is_nullable?: string;
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
  originalColType: oldColumn.udt_name,
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
  onInvalidGqlColName: () => void
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
  const table = findTable(allSchemas, tableDef) as Table;

  const {
    originalColType,
    originalData_type,
    originalColDefault,
    originalColComment,
    originalColNullable,
  } = parseOldColumns(oldColumn);

  const originalColUnique = isColumnUnique(table, colName);

  const ALTER_TABLE = `ALTER TABLE "${currentSchema}"."${tableName}"`;
  const ALTER_COL = `${ALTER_TABLE} ALTER COLUMN "${colName}"`;

  /* column type up/down migration */
  const columnChangesUpQuery = `${ALTER_COL} TYPE ${colType};`;
  const columnChangesDownQuery = `${ALTER_COL} TYPE ${originalData_type};`;

  // instantiate MIgration Helper
  const migration = new Migration();

  if (originalColType !== colType) {
    migration.add(
      getRunSqlQuery(columnChangesUpQuery),
      getRunSqlQuery(columnChangesDownQuery)
    );
  }

  /* column custom field up/down migration */
  const existingCustomColumnNames = getTableCustomColumnNames(table);
  const existingRootFields = getTableCustomRootFields(
    table
  ) as CustomRootFields;
  const newCustomColumnNames = { ...existingCustomColumnNames };
  let isCustomFieldNameChanged = false;
  if (customFieldName) {
    if (customFieldName !== existingCustomColumnNames[colName]) {
      isCustomFieldNameChanged = true;
      newCustomColumnNames[colName] = customFieldName.trim();
    }
  } else if (existingCustomColumnNames[colName]) {
    isCustomFieldNameChanged = true;
    delete newCustomColumnNames[colName];
  }
  if (isCustomFieldNameChanged) {
    migration.add(
      getSetCustomRootFieldsQuery(
        tableDef,
        existingRootFields,
        newCustomColumnNames
      ),
      getSetCustomRootFieldsQuery(
        tableDef,
        existingRootFields,
        existingCustomColumnNames
      )
    );
  }

  const colDefaultWithQuotes =
    isColTypeString(colType) &&
    !isPostgresFunction(colDefault) &&
    !isTypeCast(colDefault)
      ? `'${colDefault}'`
      : colDefault;
  const originalColDefaultWithQuotes =
    isColTypeString(colType) &&
    !isPostgresFunction(originalColDefault) &&
    !isTypeCast(originalColDefault)
      ? `'${originalColDefault}'`
      : originalColDefault;

  /* column default up/down migration */
  let columnDefaultUpQuery;
  let columnDefaultDownQuery;
  if (colDefault !== '') {
    // ALTER TABLE ONLY <table> ALTER COLUMN <column> SET DEFAULT <default>;
    columnDefaultUpQuery = `ALTER TABLE ONLY "${currentSchema}"."${tableName}" ALTER COLUMN "${colName}" SET DEFAULT ${colDefaultWithQuotes};`;
  } else {
    // ALTER TABLE <table> ALTER COLUMN <column> DROP DEFAULT;
    columnDefaultUpQuery = `${ALTER_COL} DROP DEFAULT;`;
  }

  if (originalColDefault !== '') {
    columnDefaultDownQuery = `ALTER TABLE ONLY "${currentSchema}"."${tableName}" ALTER COLUMN "${colName}" SET DEFAULT ${originalColDefaultWithQuotes};`;
  } else {
    // there was no default value originally. so drop default.
    columnDefaultDownQuery = `ALTER TABLE ONLY "${currentSchema}"."${tableName}" ALTER COLUMN "${colName}" DROP DEFAULT;`;
  }

  // check if default is unchanged and then do a drop. if not skip
  if (originalColDefault !== colDefault) {
    migration.add(
      getRunSqlQuery(columnDefaultUpQuery),
      getRunSqlQuery(columnDefaultDownQuery)
    );
  }

  /* column nullable up/down migration */
  if (nullable) {
    // ALTER TABLE <table> ALTER COLUMN <column> DROP NOT NULL;
    const nullableUpQuery = `${ALTER_COL} DROP NOT NULL;`;
    const nullableDownQuery = `${ALTER_COL} SET NOT NULL;`;
    // check with original null
    if (originalColNullable !== 'YES') {
      migration.add(
        getRunSqlQuery(nullableUpQuery),
        getRunSqlQuery(nullableDownQuery)
      );
    }
  } else {
    // ALTER TABLE <table> ALTER COLUMN <column> SET NOT NULL;
    const nullableUpQuery = `${ALTER_COL} SET NOT NULL;`;
    const nullableDownQuery = `${ALTER_COL} DROP NOT NULL;`;
    // check with original null
    if (originalColNullable !== 'NO') {
      migration.add(
        getRunSqlQuery(nullableUpQuery),
        getRunSqlQuery(nullableDownQuery)
      );
    }
  }

  /* column unique up/down migration */
  if (unique) {
    const uniqueUpQuery = `${ALTER_TABLE} ADD CONSTRAINT "${tableName}_${colName}_key" UNIQUE ("${colName}")`;
    const uniqueDownQuery = `${ALTER_TABLE} DROP CONSTRAINT "${tableName}_${colName}_key"`;
    // check with original unique
    if (!originalColUnique) {
      migration.add(
        getRunSqlQuery(uniqueUpQuery),
        getRunSqlQuery(uniqueDownQuery)
      );
    }
  } else {
    const uniqueDownQuery = `${ALTER_TABLE} ADD CONSTRAINT "${tableName}_${colName}_key" UNIQUE ("${colName}")`;
    const uniqueUpQuery = `${ALTER_TABLE} DROP CONSTRAINT "${tableName}_${colName}_key"`;
    // check with original unique
    if (originalColUnique) {
      migration.add(
        getRunSqlQuery(uniqueUpQuery),
        getRunSqlQuery(uniqueDownQuery)
      );
    }
  }

  /* column comment up/down migration */
  const columnCommentUpQuery = `COMMENT ON COLUMN "${currentSchema}"."${tableName}"."${colName}" IS ${sqlEscapeText(
    comment
  )}`;

  const columnCommentDownQuery = `COMMENT ON COLUMN "${currentSchema}"."${tableName}"."${colName}" IS ${sqlEscapeText(
    originalColComment
  )}`;

  // check if comment is unchanged and then do an update. if not skip
  if (originalColComment !== comment) {
    migration.add(
      getRunSqlQuery(columnCommentUpQuery),
      getRunSqlQuery(columnCommentDownQuery)
    );
  }

  /* rename column */
  if (newName && colName !== newName) {
    if (!gqlPattern.test(newName)) {
      onInvalidGqlColName();
    }
    migration.add(
      getRunSqlQuery(
        `${ALTER_TABLE} rename column "${colName}" to "${newName}";`
      ),
      getRunSqlQuery(
        `${ALTER_TABLE} rename column "${newName}" to "${colName}";`
      )
    );
  }
  const migrationName = `alter_table_${currentSchema}_${tableName}_alter_column_${colName}`;
  return {
    migrationName,
    migration,
  };
};
export const getDownQueryComments = (upqueries: RunSqlType[]) => {
  if (Array.isArray(upqueries) && upqueries.length >= 0) {
    let comment = `-- Could not auto-generate a down migration.
-- Please write an appropriate down migration for the SQL below:`;
    comment = upqueries.reduce(
      (acc, i) => `${acc}
-- ${i.args.sql}`,
      comment
    );
    return [getRunSqlQuery(comment)];
  }
  // all other errors
  return [];
};
