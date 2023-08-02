import { InconsistentObject } from '../../../metadata/types';
import { useSelector } from 'react-redux';

export const useInconsistentObject = () => {
  // Needed to avoid circular dependency
  const inconsistentObjects = useSelector<any>(
    state => state.metadata.inconsistentObjects
  ) as InconsistentObject[];
  return inconsistentObjects;
};
