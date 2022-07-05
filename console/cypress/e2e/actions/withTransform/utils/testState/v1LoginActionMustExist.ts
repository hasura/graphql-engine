import { readMetadata } from '../services/readMetadata';
import { deleteLoginAction } from '../services/deleteLoginAction';

/**
 * Ensure the V1 Action exists.
 */
export function v1LoginActionMustExist() {
  Cypress.log({ message: '**--- Action check: start**' });

  readMetadata().then(response => {
    const actionExists = !!response.body.actions?.find(
      // TODO: properly type it
      action => action.name === 'TODO:'
    );

    if (actionExists) {
      Cypress.log({ message: '**--- The action exists**' });
      Cypress.log({ message: '**--- Action check: end**' });
      return;
    }

    Cypress.log({ message: '**--- The action does not exist**' });

    cy.request('POST', 'http://localhost:8080/v1/metadata', {
      type: 'bulk',
      source: 'default',
      args: [
        {
          type: 'set_custom_types',
          args: {
            scalars: [],
            input_objects: [
              {
                name: 'SampleInput',
                description: null,
                fields: [
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
              },
            ],
            objects: [
              {
                name: 'SampleOutput',
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
                name: 'LoginResponse',
                fields: [
                  {
                    name: 'accessToken',
                    type: 'String!',
                  },
                ],
              },
            ],
            enums: [],
          },
        },
        {
          type: 'create_action',
          args: {
            name: 'v1Login',
            definition: {
              arguments: [
                {
                  name: 'arg1',
                  type: 'SampleInput!',
                  description: null,
                },
              ],
              kind: 'synchronous',
              output_type: 'SampleOutput',
              handler: 'http://host.docker.internal:3000',
              type: 'mutation',
              headers: [],
              timeout: null,
              request_transform: {
                version: 1,
                template_engine: 'Kriti',
                method: 'GET',
                url: '{{$base_url}}/users',
                query_params: {},
                body:
                  '{\n  "users": {\n    "name": {{$body.input.arg1.username}}\n  }\n}',
                content_type: 'application/json',
              },
            },
            comment: null,
          },
        },
      ],
    })
      .then(response => {
        expect(response.body, 'Response body exists').to.not.be.null;
        expect(response.body, 'Response body is an array').to.be.a('array');
        expect(
          response.body[0],
          'Response body contains success'
        ).to.have.property('message', 'success');
      })
      .then(() => {
        Cypress.log({ message: '**--- The action has been created**' });
        Cypress.log({ message: '**--- Action check: end**' });
      });
  });
}
