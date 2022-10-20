import { DataTarget } from '@/features/Datasources';

export const getSchemaKey = (sourceTableInfo: DataTarget) => {
  return 'dataset' in sourceTableInfo ? 'dataset' : 'schema';
};
