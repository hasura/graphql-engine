import { useFormContext } from 'react-hook-form';
import { QueryType } from '../types';

export const useIsDisabled = (queryType: QueryType) => {
  const { watch } = useFormContext();
  const checkType = watch('checkType');
  const filterType = watch('filterType');

  if (queryType === 'insert' || queryType === 'update') {
    return checkType === 'none';
  }

  return filterType === 'none';
};
