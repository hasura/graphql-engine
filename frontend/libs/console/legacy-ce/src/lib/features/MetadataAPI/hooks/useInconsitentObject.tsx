import { InconsistentObject } from '@/metadata/types';
import { useSelector } from 'react-redux';

export const useInconsistentObject = () => {
  const inconsistentObjects = useSelector<any>(
    state => state.metadata.inconsistentObjects
  ) as InconsistentObject[];
  return inconsistentObjects;
};
