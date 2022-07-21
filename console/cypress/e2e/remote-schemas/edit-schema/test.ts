/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */
import { testMode } from '../../../helpers/common';
import { setMetaData } from '../../validators/validators';
import { modifyCustomization } from './spec';

// const visitRoute = () => {
//   describe('Setup route', () => {
//     it('Visit the index route', () => {
//       // Visit the index route
//       cy.visit('/remote-schemas/manage/schemas');
//       // Get and set validation metadata
//       setMetaData();
//     });
//   });
// };

const createRemoteSchema = (remoteSchemaName: string) => {
  const postBody = {
    type: 'add_remote_schema',
    args: {
      name: remoteSchemaName,
      definition: {
        url: 'https://graphql-pokemon2.vercel.app',
        forward_client_headers: true,
        timeout_seconds: 60,
      },
    },
  };
  cy.request('POST', 'http://localhost:8080/v1/metadata', postBody).then(
    response => {
      expect(response.body).to.have.property('message', 'success'); // true
    }
  );
};

const removeRemoteSchema = (remoteSchemaName: string) => {
  const postBody = {
    type: 'remove_remote_schema',
    args: {
      name: remoteSchemaName,
    },
  };
  cy.request('POST', 'http://localhost:8080/v1/metadata', postBody).then(
    response => {
      expect(response.body).to.have.property('message', 'success'); // true
    }
  );
};

const editSchemaTests = () => {
  describe('Modify an existing remote schema', () => {
    describe('Create a remote schema for testing', () => {
      it('add a remote schema via the API', () => {
        createRemoteSchema('test_remote_schema');
      });
    });

    describe('Edit the remote schema settings', () => {
      it('Visit the modify page', () => {
        cy.visit('/remote-schemas/manage/test_remote_schema/modify');
        setMetaData();
      });

      it('Modify the remote schema settings', () => {
        modifyCustomization({
          root_fields_namespace: 'test_root_namespace',
          type_names: {
            prefix: 'test_prefix',
            suffix: 'test_suffix',
            mapping: {
              Pokemon: 'renamed_type_name_mapping',
            },
          },
          field_names: [
            {
              parent_type: 'PokemonDimension',
              prefix: 'test_parent_type_prefix',
              suffix: 'test_parent_type_suffix',
              mapping: {
                minimum: 'test_field_name',
              },
            },
          ],
        });
      });

      it('expect success notification', () => {
        cy.expectSuccessNotificationWithTitle('Remote schema modified');
      });
    });

    describe('Remove remote schema', () => {
      it('Remove the remote schema via the API', () => {
        removeRemoteSchema('test_remote_schema');
      });
    });
  });
};

if (testMode !== 'cli') {
  // setup();
  editSchemaTests();
}
