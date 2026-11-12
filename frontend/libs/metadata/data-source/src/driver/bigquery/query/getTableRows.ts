import { BigQueryTable } from '../types';
import {
  GetDefaultQueryRootProps,
  GetTableRowsProps,
  TableRow,
} from '../../types';
import { getTableRowsFromGraphQL } from '../../common/getTableRowsFromGraphQL';

export const getDefaultQueryRoot = ({ table }: GetDefaultQueryRootProps) => {
  const { name, dataset } = table as BigQueryTable;
  return `${dataset}_${name}`;
};

export const getTableRows = (props: GetTableRowsProps): Promise<TableRow[]> =>
  getTableRowsFromGraphQL({
    ...props,
    defaultQueryRoot: getDefaultQueryRoot(props),
  });
