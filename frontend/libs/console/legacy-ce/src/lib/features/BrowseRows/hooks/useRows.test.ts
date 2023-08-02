import { renderHook } from '@testing-library/react-hooks';
import { rest } from 'msw';
import { setupServer } from 'msw/node';
import { wrapper } from '../../../hooks/__tests__/common/decorator';
import { useRows, UseRowsPropType } from './useRows';
import { TableRow } from '../../DataSource';
import { Metadata } from '../../hasura-metadata-types';

describe('Postgres browse rows data', () => {
  const mockMetadata: Metadata = {
    resource_version: 54,
    metadata: {
      version: 3,
      sources: [
        {
          name: 'chinook',
          kind: 'postgres',
          tables: [
            {
              table: {
                name: 'Album',
                schema: 'public',
              },
            },
          ],
          configuration: {
            connection_info: {
              database_url:
                'postgres://postgres:test@host.docker.internal:6001/chinook',
              isolation_level: 'read-committed',
              use_prepared_statements: false,
            },
          },
        },
      ],
    },
  };

  const expectedResult: TableRow[] = [
    {
      AlbumId: 225,
      Title: 'Volume Dois',
      ArtistId: 146,
    },
    {
      AlbumId: 275,
      Title: 'Vivaldi: The Four Seasons',
      ArtistId: 209,
    },
    {
      AlbumId: 114,
      Title: 'Virtual XI',
      ArtistId: 90,
    },
    {
      AlbumId: 52,
      Title: 'Vinícius De Moraes - Sem Limite',
      ArtistId: 70,
    },
    {
      AlbumId: 247,
      Title: 'Vinicius De Moraes',
      ArtistId: 72,
    },
    {
      AlbumId: 67,
      Title: "Vault: Def Leppard's Greatest Hits",
      ArtistId: 78,
    },
    {
      AlbumId: 245,
      Title: 'Van Halen III',
      ArtistId: 152,
    },
    {
      AlbumId: 244,
      Title: 'Van Halen',
      ArtistId: 152,
    },
    {
      AlbumId: 92,
      Title: 'Use Your Illusion II',
      ArtistId: 88,
    },
    {
      AlbumId: 91,
      Title: 'Use Your Illusion I',
      ArtistId: 88,
    },
  ];

  const server = setupServer(
    rest.post('/v1/metadata', (req, res, ctx) => {
      return res(ctx.status(200), ctx.json(mockMetadata));
    }),
    rest.post('/v2/query', (req, res, ctx) => {
      return res(ctx.status(200), ctx.json(expectedResult));
    }),
    rest.post('/v1/graphql', (req, res, ctx) => {
      return res(
        ctx.status(200),
        ctx.json({
          data: {
            __schema: {
              queryType: { name: 'query_root' },
              mutationType: { name: 'mutation_root' },
              subscriptionType: { name: 'subscription_root' },
              types: [
                {
                  kind: 'OBJECT',
                  name: 'Album',
                  description: 'asdasdads',
                  fields: [
                    {
                      name: 'AlbumId',
                      description: null,
                      args: [],
                      type: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: { kind: 'SCALAR', name: 'Int', ofType: null },
                      },
                      isDeprecated: false,
                      deprecationReason: null,
                    },
                    {
                      name: 'ArtistId',
                      description: null,
                      args: [],
                      type: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: { kind: 'SCALAR', name: 'Int', ofType: null },
                      },
                      isDeprecated: false,
                      deprecationReason: null,
                    },
                    {
                      name: 'Title',
                      description: null,
                      args: [],
                      type: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'SCALAR',
                          name: 'String',
                          ofType: null,
                        },
                      },
                      isDeprecated: false,
                      deprecationReason: null,
                    },
                  ],
                  inputFields: null,
                  interfaces: [],
                  enumValues: null,
                  possibleTypes: null,
                },
              ],
            },
          },
        })
      );
    })
  );

  beforeAll(() => {
    server.listen();
  });
  afterAll(() => {
    server.close();
  });

  it('returns table data for a postgres table', async () => {
    const props: UseRowsPropType = {
      dataSourceName: 'chinook',
      table: { name: 'Album', schema: 'public' },
      options: {
        limit: 10,
        where: [{ AlbumId: { $gt: 4 } }],
        order_by: [{ column: 'Title', type: 'desc' }],
        offset: 15,
      },
    };
    const { result, waitFor } = renderHook(() => useRows(props), { wrapper });
    await waitFor(() => result.current.isSuccess);
    expect(result.current.data).toEqual(expectedResult);
  });
});

//   /** The DAL method for this is a work in progress. this test will updated later */

// describe('MSSQL browse rows', () => {
//   const mockMetadata: Metadata = {
//     resource_version: 54,
//     metadata: {
//       version: 3,
//       sources: [
//         {
//           name: 'bikes',
//           kind: 'mssql',
//           tables: [
//             {
//               table: {
//                 name: 'customers',
//                 schema: 'sales',
//               },
//             },
//           ],
//           configuration: {
//             connection_info: {
//               connection_string:
//                 'DRIVER={ODBC Driver 18 for SQL Server};SERVER=host.docker.internal;DATABASE=bikes;Uid=SA;Pwd=reallyStrongPwd123;Encrypt=optional',
//               pool_settings: {
//                 idle_timeout: 5,
//                 max_connections: 50,
//               },
//             },
//           },
//         },
//       ],
//     },
//   };

//   const expectedResult: TableRow[] = [
//     {
//       customer_id: 259,
//       order_date: '2016-01-01',
//       order_id: 1,
//       order_status: 4,
//       required_date: '2016-01-03',
//       shipped_date: '2016-01-03',
//       staff_id: 2,
//       store_id: 1,
//     },
//     {
//       customer_id: 1212,
//       order_date: '2016-01-01',
//       order_id: 2,
//       order_status: 4,
//       required_date: '2016-01-04',
//       shipped_date: '2016-01-03',
//       staff_id: 6,
//       store_id: 2,
//     },
//     {
//       customer_id: 523,
//       order_date: '2016-01-02',
//       order_id: 3,
//       order_status: 4,
//       required_date: '2016-01-05',
//       shipped_date: '2016-01-03',
//       staff_id: 7,
//       store_id: 2,
//     },
//   ];

//   const server = setupServer(
//     rest.post('http://localhost/v1/metadata', (req, res, ctx) => {
//       return res(ctx.status(200), ctx.json(mockMetadata));
//     }),
//     rest.post('http://localhost/v2/query', (req, res, ctx) => {
//       return res(
//         ctx.status(200),
//         ctx.json({
//           result_type: 'TuplesOk',
//           result: [
//             ['COLUMN_NAME', 'DATA_TYPE'],
//             ['store_id', 'int'],
//             ['store_name', 'varchar'],
//             ['phone', 'varchar'],
//             ['email', 'varchar'],
//             ['street', 'varchar'],
//             ['city', 'varchar'],
//             ['state', 'varchar'],
//             ['zip_code', 'varchar'],
//           ],
//         })
//       );
//     }),
//     rest.post('http://localhost/v1/graphql', (req, res, ctx) => {
//       return res(
//         ctx.status(200),
//         ctx.json({ data: { sales_customers: expectedResult } })
//       );
//     })
//   );

//   beforeAll(() => {
//     server.listen();
//   });
//   afterAll(() => {
//     server.close();
//   });

//   // it('returns table data for an mssql table', async () => {
//   //   const props: UseRowsPropType = {
//   //     dataSourceName: 'bikes',
//   //     table: { name: 'customers', schema: 'sales' },
//   //   };
//   //   const { result, waitFor } = renderHook(() => useRows(props), { wrapper });
//   //   await waitFor(() => result.current.isSuccess);
//   //   expect(result.current.data).toEqual(expectedResult);
//   // });
// });
