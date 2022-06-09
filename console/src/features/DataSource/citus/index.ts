import { getValidationSchema } from './connectDB/getValidationSchema';
import { getUISchema } from './connectDB/getUISchema';
import { Database } from '..';

export const citus: Database = {
  connectDB: {
    getUISchema,
    getValidationSchema,
  },
};
