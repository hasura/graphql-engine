import { PostgresFamilyDriver } from '@hasura/shared/types';
import { Database } from '../../types/database';
import { getVersionCurry } from './getVersion';
import {
  getPostgresFunctionCommentCurry,
  getPostgresTableCommentCurry,
  getPostgresViewCommentCurry,
} from './getTableComment';
import { getFKRelationshipsCurry } from './getFKRelationships';
import { getPostgresViewDefinitionCurry } from './getViewDefinition';
import { getCheckConstraintsCurry } from './getCheckConstraints';
import { getPrimaryKeyCurry, getUniqueKeysCurry } from './getTableKeys';
import { getTableIndexesCurry } from './getTableIndexes';
import {
  getTableTriggersCurry,
  getTriggerFunctionsCurry,
} from './getTableTriggers';

export { getTrackableTables } from './getTrackableTables';
// export { getDatabaseConfiguration } from './getDatabaseConfiguration';
export { getTableColumns, getTableColumnInfos } from './getTableColumns';
export * from './getFKRelationships';
export { getTablesListAsTree } from './getTablesListAsTree';
export { getSupportedOperators } from './getSupportedOperators';
export { getTrackableFunctions } from './getTrackableFunctions';
export { getDatabaseSchemas } from './getDatabaseSchemas';

export * from './getIsTableView';
export * from './getCheckConstraints';
export * from './getVersion';
export * from './getFunctionDefinition';
export * from './getTableConstraintDefinition';

export const getPostgresIntrospectionMethods = (
  driver: PostgresFamilyDriver,
): Partial<Database['introspection']> => ({
  getVersion: getVersionCurry(driver),
  getTableComment: getPostgresTableCommentCurry(driver),
  getViewComment: getPostgresViewCommentCurry(driver),
  getFunctionComment: getPostgresFunctionCommentCurry(driver),
  getFKRelationships: getFKRelationshipsCurry(driver),
  getViewDefinition: getPostgresViewDefinitionCurry(driver),
  getCheckConstraints: getCheckConstraintsCurry(driver),
  getPrimaryKey: getPrimaryKeyCurry(driver),
  getUniqueKeys: getUniqueKeysCurry(driver),
  getTableIndexes: getTableIndexesCurry(driver),
  getTableTriggers: getTableTriggersCurry(driver),
  getTriggerFunctions: getTriggerFunctionsCurry(driver),
});
export * from './adaptCheckConstraints';
export * from './adaptKeys';
export * from './getTableKeys';
export * from './adaptIndexes';
export * from './getTableIndexes';
export * from './adaptTriggers';
export * from './getTableTriggers';
