import { getDatabaseMethods } from '../dataSources';
import { supportsSchemaLessTables } from '../../utils/capabilities';

// Regression: native SQL drivers must NOT report schemaless/collection-table
// support. When postgresCapabilities.data_schema.supports_schemaless_tables was
// `true`, TableRow routed the Track button to the Mongo "Track Collection"
// modal for plain PostgreSQL tables, so trackTables was never called.
describe('native SQL driver capabilities — supports_schemaless_tables', () => {
  const sqlDrivers = ['postgres', 'pg', 'citus', 'cockroach', 'mssql'];

  it.each(sqlDrivers)(
    '%s getDriverCapabilities() reports supports_schemaless_tables = false',
    async (driver) => {
      const capabilities =
        await getDatabaseMethods(driver).introspection.getDriverCapabilities();

      expect(capabilities?.data_schema?.supports_schemaless_tables).toBe(false);
      // ...and the UI helper that gates the Track flow agrees.
      expect(supportsSchemaLessTables(capabilities)).toBe(false);
    },
  );

  it('supportsSchemaLessTables still returns true for a genuinely schemaless (Mongo/GDC) capability', () => {
    // Mongo/GDC capabilities come from the connector at runtime; the helper must
    // keep honouring a real `true` so their Track-collection flow is preserved.
    expect(
      supportsSchemaLessTables({
        data_schema: { supports_schemaless_tables: true },
      }),
    ).toBe(true);
  });

  it('supportsSchemaLessTables defaults to false when capabilities are absent', () => {
    expect(supportsSchemaLessTables(undefined)).toBe(false);
  });
});
