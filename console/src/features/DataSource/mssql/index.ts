import { getValidationSchema } from './connectDB/getValidationSchema';
import { getUISchema } from './connectDB/getUISchema';
import { Database } from '..';

export const mssql: Database = {
  connectDB: {
    getUISchema,
    getValidationSchema,
  },
};
