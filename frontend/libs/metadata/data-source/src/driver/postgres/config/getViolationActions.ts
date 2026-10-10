import { ViolationAction } from '../../types';

export const getViolationActions = (): ViolationAction[] => {
  return ['restrict', 'no action', 'cascade', 'set null', 'set default'];
};
