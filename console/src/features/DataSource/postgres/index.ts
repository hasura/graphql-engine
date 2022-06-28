import { getConfigSchema } from './connectDB/getConfigSchema';

import { Database } from '..';

export const postgres: Database = {
  connectDB: {
    getConfigSchema,
  },
};
