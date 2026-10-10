import { Database } from '../types/database';
import { postgres } from '../postgres/database';

export const alloy: Database = {
  ...postgres,
  introspection: {
    ...postgres.introspection!,
    getDriverInfo: async () => ({
      name: 'alloy',
      displayName: 'AlloyDB',
      release: 'GA',
    }),
  },
};
