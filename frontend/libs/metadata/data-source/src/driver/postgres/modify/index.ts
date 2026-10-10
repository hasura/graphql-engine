import { PostgresFamilyDriver } from '@hasura/shared/types';
import { createDatabaseSchemaCurry } from './createDatabaseSchema';
import { deleteDatabaseSchemaCurry } from './deleteDatabaseSchema';
import { dropFunctionCurry } from './dropFunction';
import { createForeignKeyCurry } from './createForeignKey';
import { alterForeignKeyCurry } from './alterForeignKey';
import { dropForeignKeyCurry } from './dropForeignKey';
import {
  createCheckConstraintCurry,
  dropCheckConstraintCurry,
} from './checkConstraint';
import {
  createPrimaryKeyCurry,
  alterPrimaryKeyCurry,
  dropPrimaryKeyCurry,
} from './primaryKey';
import { createUniqueKeyCurry, dropUniqueKeyCurry } from './uniqueKey';
import { createIndexCurry, dropIndexCurry } from './tableIndex';
import { createTriggerCurry, dropTriggerCurry } from './trigger';
import { Database } from '../../types/database';
import { dropTableCurry } from './dropTable';
import { changeTableNameCurry } from './changeTableName';
import { alterColumnCurry } from './alterColumn';
import { addColumnCurry } from './addColumn';
import { dropColumnCurry } from './dropColumn';

export * from './createDatabaseSchema';
export * from './deleteDatabaseSchema';
export * from './updateRow';
export * from './insertRows';
export * from './alterForeignKey';
export * from './checkConstraint';
export * from './primaryKey';
export * from './uniqueKey';
export * from './tableIndex';
export * from './trigger';
export * from './dropTable';
export * from './alterColumn';
export * from './addColumn';
export * from './dropColumn';

import { insertRows } from './insertRows';
import { updateRows } from './updateRow';
import { deleteRows } from './deleteRows';
import { createTableCurry } from './createTable';

export const createPostgresModifyMethods = (
  driver: PostgresFamilyDriver,
): Database['modify'] => ({
  createDatabaseSchema: createDatabaseSchemaCurry(driver),
  deleteDatabaseSchema: deleteDatabaseSchemaCurry(driver),
  dropFunction: dropFunctionCurry(driver),
  createForeignKey: createForeignKeyCurry(driver),
  alterForeignKey: alterForeignKeyCurry(driver),
  dropForeignKey: dropForeignKeyCurry(driver),
  createCheckConstraint: createCheckConstraintCurry(driver),
  dropCheckConstraint: dropCheckConstraintCurry(driver),
  createPrimaryKey: createPrimaryKeyCurry(driver),
  alterPrimaryKey: alterPrimaryKeyCurry(driver),
  dropPrimaryKey: dropPrimaryKeyCurry(driver),
  createUniqueKey: createUniqueKeyCurry(driver),
  dropUniqueKey: dropUniqueKeyCurry(driver),
  createIndex: createIndexCurry(driver),
  dropIndex: dropIndexCurry(driver),
  createTrigger: createTriggerCurry(driver),
  dropTrigger: dropTriggerCurry(driver),
  dropTable: dropTableCurry(driver),
  changeTableName: changeTableNameCurry(driver),
  addColumn: addColumnCurry(driver),
  alterColumn: alterColumnCurry(driver),
  dropColumn: dropColumnCurry(driver),
  createTable: createTableCurry(driver),
  insertRows,
  updateRows,
  deleteRows,
});
