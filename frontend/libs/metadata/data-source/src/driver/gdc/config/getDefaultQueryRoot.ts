import { GetDefaultQueryRootProps } from '../../types';

export const getDefaultQueryRoot = ({ table }: GetDefaultQueryRootProps) => {
  if (typeof table === 'string') {
    return table;
  }

  if (Array.isArray(table)) {
    return table.join('_');
  }

  throw Error('getTrackableTables: `table` is not a string or string[]');
};
