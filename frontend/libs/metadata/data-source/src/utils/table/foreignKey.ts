import { getTableDisplayName } from '@hasura/shared/utils';
import { TableFkRelationships } from '../../driver/types';

export const displayForeignKey = (foreignKey: TableFkRelationships) => {
  const lCol =
    foreignKey.from.columns.length > 1
      ? '( ' + foreignKey.from.columns.join(', ') + ' )'
      : foreignKey.from.columns[0];
  const rCol =
    foreignKey.to.columns.length > 1
      ? '( ' + foreignKey.to.columns.join(', ') + ' )'
      : foreignKey.to.columns[0];

  return `${lCol} → ${getTableDisplayName(foreignKey.to.table)} . ${rCol}`;
};
