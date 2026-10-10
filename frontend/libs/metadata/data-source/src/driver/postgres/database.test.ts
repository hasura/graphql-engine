import { postgres } from './database';

describe('postgres driver', () => {
  it('exposes the shared Postgres-family introspection methods', () => {
    const { introspection } = postgres;
    expect(introspection.getPrimaryKey).toBeDefined();
    expect(introspection.getUniqueKeys).toBeDefined();
    expect(introspection.getCheckConstraints).toBeDefined();
    expect(introspection.getTableIndexes).toBeDefined();
    expect(introspection.getTableTriggers).toBeDefined();
  });
});
