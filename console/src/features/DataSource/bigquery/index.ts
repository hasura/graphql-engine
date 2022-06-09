import { Database } from '..';
import { getUISchema } from './connectDB/getUISchema';
import { getValidationSchema } from './connectDB/getValidationSchema';

export const bigquery: Database = {
  connectDB: {
    getUISchema,
    getValidationSchema,
  },
};
