import { TableRow, WhereClause } from '@/features/DataSource';

export type AdaptSelectedRowIdsToWhereClauseArgs = {
  rowsId: Record<number, boolean>;
  rows: TableRow[] | undefined;
  primaryKeys: string[];
};

export const adaptSelectedRowIdsToWhereClause = ({
  rowsId,
  rows,
  primaryKeys,
}: AdaptSelectedRowIdsToWhereClauseArgs): WhereClause[] => {
  const selectedRows = (rows || []).filter(
    (row, index) => rowsId[index] === true
  );

  const baseObject: WhereClause[] = primaryKeys.map(primaryKey => ({
    [primaryKey]: { _in: [] },
  }));

  const whereClause: WhereClause[] = selectedRows.reduce((acc, row) => {
    primaryKeys.forEach(primaryKey => {
      // eslint-disable-next-line no-underscore-dangle
      const primaryKeyObject = acc.find(
        val => Object.keys(val)[0] === primaryKey
      );
      if (primaryKeyObject) {
        const value = row[primaryKey];
        // eslint-disable-next-line no-underscore-dangle
        const array = primaryKeyObject[primaryKey]._in as unknown[];

        array.push(value);
      }
    });

    return [...acc];
  }, baseObject);

  return whereClause;
};
