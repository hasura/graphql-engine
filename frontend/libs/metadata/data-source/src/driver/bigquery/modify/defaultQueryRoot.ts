import { GetDefaultQueryRootProps } from '../../types';
import { BigQueryTable } from '../types';

export const defaultQueryRoot = async ({ table }: GetDefaultQueryRootProps) => {
  const { name, dataset } = table as BigQueryTable;

  return `${dataset}_${name}`;
};
