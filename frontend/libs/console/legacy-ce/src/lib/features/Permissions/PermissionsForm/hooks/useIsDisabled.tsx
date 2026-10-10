import { useFormContext } from 'react-hook-form';
import type { DataQueryType } from '@hasura/shared/types';

export const useIsDisabled = (queryType: DataQueryType) => {
  const { watch } = useFormContext();
  const checkType = watch('checkType');
  const filterType = watch('filterType');

  if (queryType === 'insert') {
    return checkType === 'none';
  }

  if (queryType === 'update') {
    return filterType === 'none';
  }

  return filterType === 'none';
};
