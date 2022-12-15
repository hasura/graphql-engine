import { useAppSelector } from '@/store';

export const useInconsistentObject = () => {
  const inconsistentObjects = useAppSelector(
    state => state.metadata.inconsistentObjects
  );
  return inconsistentObjects;
};
