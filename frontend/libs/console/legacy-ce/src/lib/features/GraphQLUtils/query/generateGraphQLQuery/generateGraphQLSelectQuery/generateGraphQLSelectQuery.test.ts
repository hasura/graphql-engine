import { MetadataTable, Source } from '../../../../hasura-metadata-types';
import { generateGraphQLSelectQuery } from './generateGraphQLSelectQuery';

describe('GraphQL utils for browse rows - ', () => {
  describe('[select] a table with no customization', () => {
    it('Generates correct GraphQL query', async () => {
      const { query } = await generateGraphQLSelectQuery({
        defaultQueryRoot: 'sales_stores',
        columns: [
          'store_id',
          'store_name',
          'phone',
          'email',
          'street',
          'city',
          'state',
          'zip_code',
        ],
        tableCustomization: undefined,
        sourceCustomization: undefined,
      });

      expect(query).toMatchInlineSnapshot(`
        "query MyQuery {
              sales_stores  {
                store_id 
        ,store_name 
        ,phone 
        ,email 
        ,street 
        ,city 
        ,state 
        ,zip_code 

              }
            }"
      `);
    });
  });

  describe('[select] a table with table name customization', () => {
    it('Generates correct GraphQL query', async () => {
      const tableCustomization: MetadataTable['configuration'] = {
        column_config: {},
        custom_column_names: {},
        custom_name: 'CustomNameForTable',
        custom_root_fields: {},
      };

      const { query } = await generateGraphQLSelectQuery({
        defaultQueryRoot: 'sales_stores',
        columns: [
          'store_id',
          'store_name',
          'phone',
          'email',
          'street',
          'city',
          'state',
          'zip_code',
        ],
        tableCustomization,
        sourceCustomization: undefined,
      });

      expect(query).toMatchInlineSnapshot(`
        "query MyQuery {
              CustomNameForTable  {
                store_id 
        ,store_name 
        ,phone 
        ,email 
        ,street 
        ,city 
        ,state 
        ,zip_code 

              }
            }"
      `);
    });
  });

  describe('[select] a table with select query customization', () => {
    it('Generates correct GraphQL query', async () => {
      const tableCustomization: MetadataTable['configuration'] = {
        column_config: {},
        custom_column_names: {},
        custom_root_fields: { select: 'CustomSelectQueryName' },
      };
      const { query } = await generateGraphQLSelectQuery({
        defaultQueryRoot: 'sales_stores',
        columns: [
          'store_id',
          'store_name',
          'phone',
          'email',
          'street',
          'city',
          'state',
          'zip_code',
        ],
        tableCustomization,
        sourceCustomization: {},
      });

      expect(query).toMatchInlineSnapshot(`
        "query MyQuery {
              CustomSelectQueryName  {
                store_id 
        ,store_name 
        ,phone 
        ,email 
        ,street 
        ,city 
        ,state 
        ,zip_code 

              }
            }"
      `);
    });
  });

  describe('[select] a table with custom field names', () => {
    it('Generates correct GraphQL query', async () => {
      const tableCustomization: MetadataTable['configuration'] = {
        column_config: {
          phone: {
            custom_name: 'customNameForPhone',
          },
          email: {
            custom_name: 'customNameForEmail',
          },
          street: {
            custom_name: 'customNameForStreet',
          },
        },
        custom_column_names: {
          phone: 'customNameForPhone',
          email: 'customNameForEmail',
          street: 'customNameForStreet',
        },
        custom_root_fields: {},
      };
      const { query } = await generateGraphQLSelectQuery({
        defaultQueryRoot: 'sales_stores',
        columns: [
          'store_id',
          'store_name',
          'phone',
          'email',
          'street',
          'city',
          'state',
          'zip_code',
        ],
        tableCustomization,
        sourceCustomization: {},
      });

      expect(query).toMatchInlineSnapshot(`
        "query MyQuery {
              sales_stores  {
                store_id 
        ,store_name 
        ,customNameForPhone 
        ,customNameForEmail 
        ,customNameForStreet 
        ,city 
        ,state 
        ,zip_code 

              }
            }"
      `);
    });
  });

  describe('[select] a table with source level customization (namespace, prefix & suffix)', () => {
    it('Generates correct GraphQL query', async () => {
      const sourceCustomization: Source['customization'] = {
        root_fields: {
          namespace: 'SourceNamespace_',
          prefix: 'TablePrefix_',
          suffix: '_TableSuffix',
        },
      };
      const { query } = await generateGraphQLSelectQuery({
        defaultQueryRoot: 'sales_stores',
        columns: [
          'store_id',
          'store_name',
          'phone',
          'email',
          'street',
          'city',
          'state',
          'zip_code',
        ],
        tableCustomization: {},
        sourceCustomization,
      });

      expect(query).toMatchInlineSnapshot(`
        "query MyQuery  {
                SourceNamespace_  {
                  TablePrefix_sales_stores_TableSuffix  {
                    store_id 
        ,store_name 
        ,phone 
        ,email 
        ,street 
        ,city 
        ,state 
        ,zip_code 

                  }
                }
              }"
      `);
    });
  });

  describe('[select] a table with all possible customizations at table, source (namespace, prefix & suffix) & column level (no custom query root)', () => {
    it('Generates correct GraphQL query', async () => {
      const tableCustomization: MetadataTable['configuration'] = {
        column_config: {
          phone: {
            custom_name: 'customNameForPhone',
          },
          email: {
            custom_name: 'customNameForEmail',
          },
          street: {
            custom_name: 'customNameForStreet',
          },
        },
        custom_column_names: {
          phone: 'customNameForPhone',
          email: 'customNameForEmail',
          street: 'customNameForStreet',
        },
        custom_name: 'CustomNameForTable',
        custom_root_fields: {},
      };

      const sourceCustomization: Source['customization'] = {
        root_fields: {
          namespace: 'SourceNamespace_',
          prefix: 'TablePrefix_',
          suffix: '_TableSuffix',
        },
      };
      const { query } = await generateGraphQLSelectQuery({
        defaultQueryRoot: 'sales_stores',
        columns: [
          'store_id',
          'store_name',
          'phone',
          'email',
          'street',
          'city',
          'state',
          'zip_code',
        ],
        tableCustomization,
        sourceCustomization,
      });

      expect(query).toMatchInlineSnapshot(`
        "query MyQuery  {
                SourceNamespace_  {
                  TablePrefix_CustomNameForTable_TableSuffix  {
                    store_id 
        ,store_name 
        ,customNameForPhone 
        ,customNameForEmail 
        ,customNameForStreet 
        ,city 
        ,state 
        ,zip_code 

                  }
                }
              }"
      `);
    });
  });

  describe('[select] a table with all possible customizations at table, source (namespace, prefix & suffix) & column level (with custom query root)', () => {
    it('Generates correct GraphQL query', async () => {
      const tableCustomization: MetadataTable['configuration'] = {
        column_config: {
          phone: {
            custom_name: 'customNameForPhone',
          },
          email: {
            custom_name: 'customNameForEmail',
          },
          street: {
            custom_name: 'customNameForStreet',
          },
        },
        custom_column_names: {
          phone: 'customNameForPhone',
          email: 'customNameForEmail',
          street: 'customNameForStreet',
        },
        custom_name: 'CustomNameForTable',
        custom_root_fields: { select: 'CustomSelectQueryName' },
      };

      const sourceCustomization: Source['customization'] = {
        root_fields: {
          namespace: 'SourceNamespace_',
          prefix: 'TablePrefix_',
          suffix: '_TableSuffix',
        },
      };

      const { query } = await generateGraphQLSelectQuery({
        defaultQueryRoot: 'sales_stores',
        columns: [
          'store_id',
          'store_name',
          'phone',
          'email',
          'street',
          'city',
          'state',
          'zip_code',
        ],
        tableCustomization,
        sourceCustomization,
      });

      expect(query).toMatchInlineSnapshot(`
        "query MyQuery  {
                SourceNamespace_  {
                  TablePrefix_CustomSelectQueryName_TableSuffix  {
                    store_id 
        ,store_name 
        ,customNameForPhone 
        ,customNameForEmail 
        ,customNameForStreet 
        ,city 
        ,state 
        ,zip_code 

                  }
                }
              }"
      `);
    });
  });

  describe('[select] a table with source & table level customzations + options', () => {
    it('Generates correct GraphQL query', async () => {
      const tableCustomization: MetadataTable['configuration'] = {
        column_config: {
          phone: {
            custom_name: 'customNameForPhone',
          },
          email: {
            custom_name: 'customNameForEmail',
          },
          street: {
            custom_name: 'customNameForStreet',
          },
        },
        custom_column_names: {
          phone: 'customNameForPhone',
          email: 'customNameForEmail',
          street: 'customNameForStreet',
        },
        custom_name: 'CustomNameForTable',
        custom_root_fields: { select: 'CustomSelectQueryName' },
      };

      const sourceCustomization: Source['customization'] = {
        root_fields: {
          namespace: 'SourceNamespace_',
          prefix: 'TablePrefix_',
          suffix: '_TableSuffix',
        },
      };

      const { query } = await generateGraphQLSelectQuery({
        defaultQueryRoot: 'sales_stores',
        columns: [
          'store_id',
          'store_name',
          'phone',
          'email',
          'street',
          'city',
          'state',
          'zip_code',
        ],
        tableCustomization,
        sourceCustomization,
        options: {
          limit: 10,
          where: [
            { store_id: { _eq: 4 } },
            { street: { _eq: 'some street name' } },
          ],
          order_by: [{ column: 'street', type: 'desc' }],
          offset: 15,
        },
      });

      expect(query).toMatchInlineSnapshot(`
        "query MyQuery  {
                SourceNamespace_  {
                  TablePrefix_CustomSelectQueryName_TableSuffix (where: {store_id: { _eq: 4},customNameForStreet: { _eq: "some street name"}},order_by: {customNameForStreet: desc},limit: 10,offset: 15) {
                    store_id 
        ,store_name 
        ,customNameForPhone 
        ,customNameForEmail 
        ,customNameForStreet 
        ,city 
        ,state 
        ,zip_code 

                  }
                }
              }"
      `);
    });
  });
});
