import { getValidationSchema } from './connectDB/getValidationSchema';
import { getUISchema } from './connectDB/getUISchema';
import { Database } from '..';

export const postgres: Database = {
  connectDB: {
    getUISchema,
    getValidationSchema,
  },
};
