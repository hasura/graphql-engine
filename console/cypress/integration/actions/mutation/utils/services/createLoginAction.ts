/**
 * Create the Action straight on the server.
 */
export function createLoginAction() {
  Cypress.log({ message: '**--- Action creation: start**' });

  cy.request('POST', 'http://localhost:8080/v1/metadata', {
    type: 'bulk',
    source: 'default',
    // resource_version: 138,
    args: [
      {
        type: 'set_custom_types',
        args: {
          scalars: [],
          input_objects: [
            {
              name: 'SampleInput',
              fields: [
                { name: 'username', type: 'String!' },
                { name: 'password', type: 'String!' },
              ],
            },
          ],
          objects: [
            {
              name: 'SampleOutput',
              fields: [{ name: 'accessToken', type: 'String!' }],
            },
            {
              name: 'LoginResponse',
              description: null,
              fields: [
                {
                  name: 'accessToken',
                  type: 'String!',
                  description: null,
                },
              ],
            },
            {
              name: 'AddResult',
              fields: [{ name: 'sum', type: 'Int' }],
            },
          ],
          enums: [],
        },
      },
      {
        type: 'create_action',
        args: {
          name: 'login',
          definition: {
            arguments: [
              {
                name: 'username',
                type: 'String!',
                description: null,
              },
              {
                name: 'password',
                type: 'String!',
                description: null,
              },
            ],
            kind: 'synchronous',
            output_type: 'LoginResponse',
            handler: 'https://hasura-actions-demo.glitch.me/login',
            type: 'mutation',
            headers: [],
            timeout: 25,
            request_transform: null,
          },
          comment: null,
        },
      },
    ],
  }).then(() => Cypress.log({ message: '**--- Action creation: end**' }));
}
