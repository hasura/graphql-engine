import { MODE } from '../types';

export const NOTIFICATIONS = Object.freeze({
  onSuccess: {
    [MODE.CREATE]: 'Relationship was created successfully!',
    [MODE.EDIT]: 'Relationship was updated successfully!',
    [MODE.DELETE]: 'Relationship was deleted successfully!',
    [MODE.RENAME]: 'Relationship was renamed successfully!',
  },
  onError: {
    [MODE.CREATE]: 'Relationship could not be created!',
    [MODE.EDIT]: 'Relationship could not be updated!',
    [MODE.DELETE]: 'Relationship could not be deleted!',
    [MODE.RENAME]: 'Relationship could not be renamed!',
  },
});
