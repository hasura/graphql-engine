import { GetDefaultQueryRootProps } from '../../types';
import { PostgresTable } from '../types';

export const getDefaultQueryRoot = ({
  table,
}: GetDefaultQueryRootProps): string => {
  const { name, schema } = table as PostgresTable;

  return schema === 'public' ? name : `${schema}_${name}`;
};
