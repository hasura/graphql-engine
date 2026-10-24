import { GetTableRowsProps, TableRow } from '../../types';
import { getTableRowsFromGraphQL } from '../../common/getTableRowsFromGraphQL';
import { getDefaultQueryRoot } from '../config';

export const getTableRows = (props: GetTableRowsProps): Promise<TableRow[]> =>
  getTableRowsFromGraphQL({
    ...props,
    defaultQueryRoot: getDefaultQueryRoot({ table: props.table }),
  });
