import { getElementFromAlias } from './dataHelpers';

export const togglePayloadTransformSection = () => {
  cy.getBySel('toggle-payload-transform').click({
    force: true,
  });
};

export const toggleContextArea = () => {
  cy.getBySel('toggle-context-area').click({
    force: true,
  });
};

export const toggleRequestTransformSection = () => {
  cy.getBySel('toggle-request-transform').click({
    force: true,
  });
};

export const clearRequestUrl = () => {
  cy.get(
    getElementFromAlias('transform-requestUrl')
  ).type('{selectall}{backspace}', { force: true });
};

export const typeIntoRequestUrl = (content: string) => {
  cy.getBySel('transform-requestUrl').type(content, {
    parseSpecialCharSequences: false,
  });
};

export const checkTransformRequestUrlError = (
  exists: boolean,
  error?: string
) => {
  if (exists) {
    if (error) {
      cy.getBySel('transform-requestUrl-error')
        .should('exist')
        .and('contain', error);
    } else {
      cy.getBySel('transform-requestUrl-error').should('exist');
    }
  } else {
    cy.getBySel('transform-requestUrl-error').should('not.exist');
  }
};

export const typeIntoContextAreaEnvVars = (
  envVars: { key: string; value: string }[]
) => {
  envVars.forEach((q, i) => {
    cy.getBySel(`transform-env-vars-kv-key-${i}`).type(q.key, {
      parseSpecialCharSequences: false,
    });
    cy.getBySel(`transform-env-vars-kv-value-${i}`).type(q.value, {
      parseSpecialCharSequences: false,
    });
  });
};

export const typeIntoRequestQueryParams = (
  queryParams: { key: string; value: string }[]
) => {
  queryParams.forEach((q, i) => {
    cy.getBySel(`transform-query-params-kv-key-${i}`).type(q.key, {
      parseSpecialCharSequences: false,
    });
    cy.getBySel(`transform-query-params-kv-value-${i}`).type(q.value, {
      parseSpecialCharSequences: false,
    });
  });
};

export const checkTransformRequestUrlPreview = (previewText: string) => {
  cy.getBySel('transform-requestUrl-preview').should('have.value', previewText);
};

export const clearPayloadTransformBody = (textArea: number) => {
  cy.get('textarea').eq(textArea).type('{selectall}', { force: true });
  cy.get('textarea').eq(textArea).trigger('keydown', {
    keyCode: 46,
    which: 46,
    force: true,
  });
};

export const typeIntoTransformBody = (content: string, textArea: number) => {
  cy.get('textarea')
    .eq(textArea)
    .type(content, { force: true, parseSpecialCharSequences: false });
};

export const checkTransformRequestBodyError = (exists: boolean) => {
  if (exists) {
    cy.getBySel('transform-requestBody-error').should('exist');
  } else {
    cy.getBySel('transform-requestBody-error').should('not.exist');
  }
};

export const getActionTransfromV1RequestBody = (actionName: string) => ({
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
        name: actionName,
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
});
