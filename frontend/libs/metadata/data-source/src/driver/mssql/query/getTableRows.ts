import { MssqlTable } from '../types';
import {
  GetDefaultQueryRootProps,
  GetTableRowsProps,
  TableRow,
} from '../../types';
import { getTableRowsFromGraphQL } from '../../common/getTableRowsFromGraphQL';

export const getDefaultQueryRoot = ({ table }: GetDefaultQueryRootProps) => {
  const { name, schema } = table as MssqlTable;
  return schema === 'dbo' ? name : `${schema}_${name}`;
};

export const getTableRows = (props: GetTableRowsProps): Promise<TableRow[]> =>
  getTableRowsFromGraphQL({
    ...props,
    defaultQueryRoot: getDefaultQueryRoot(props),
  });
