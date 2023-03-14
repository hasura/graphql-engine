import React from 'react';
import z from 'zod';

import { SimpleForm } from '../../../../../../new-components/Form';
import { ConnectPostgresModal } from './ConnectPostgresModal';
import { DynamicDBRoutingForm } from './DynamicDBRoutingForm';
import { generatePostgresRequestPayload } from '../../utils/generateRequests';
import { adaptPostgresConnectionInfo } from '../../utils/adaptResponse';
import { useDynamicDbRouting } from './hooks/useDynamicDbRouting';
import { ConnectionSet } from '../../../../../../metadata/types';
import { PostgresConfiguration } from '../../../../../hasura-metadata-types';
import { hasuraToast } from '../../../../../../new-components/Toasts';

const schema = z.object({
  connection_template: z.string().optional(),
});

interface DynamicDBRoutingProps {
  sourceName: string;
}

export const DynamicDBRouting = (props: DynamicDBRoutingProps) => {
  const {
    connectionTemplate,
    connectionSet,
    addConnection,
    removeConnection,
    updateConnection,
    updateConnectionTemplate,
    isLoading,
    isMetadaLoading,
  } = useDynamicDbRouting({
    sourceName: props.sourceName,
  });

  const [isModalOpen, setIsModalOpen] = React.useState(false);
  const [editingConnectionSetMember, setEditingConnectionSetMember] =
    React.useState<string>();

  const connectionSetMembers = connectionSet.map(connection => {
    const { name, connection_info } = connection;
    return {
      name,
      configuration: {
        connectionInfo: adaptPostgresConnectionInfo(
          connection_info as PostgresConfiguration['connection_info']
        ),
      },
    };
  });

  if (isMetadaLoading) {
    return null;
  }

  return (
    <>
      {isModalOpen && (
        <ConnectPostgresModal
          alreadyUseNames={connectionSetMembers.map(
            connection => connection.name
          )}
          onSubmit={values => {
            const payload = {
              name: values.name,
              connection_info: generatePostgresRequestPayload({
                driver: 'postgres',
                values,
              }).details.configuration.connection_info,
            } as ConnectionSet;
            addConnection(payload, {
              onSuccess: () => {
                hasuraToast({
                  type: 'success',
                  title: 'Connection added',
                });
              },
              onError: e => {
                hasuraToast({
                  type: 'error',
                  title: 'Failed to add connection',
                  message: e.message,
                });
              },
            });
            setIsModalOpen(false);
          }}
          onClose={() => setIsModalOpen(false)}
        />
      )}
      {editingConnectionSetMember && (
        <ConnectPostgresModal
          alreadyUseNames={connectionSetMembers
            .map(connection => connection.name)
            .filter(name => name !== editingConnectionSetMember)}
          defaultValues={connectionSetMembers.find(
            connection => connection.name === editingConnectionSetMember
          )}
          onSubmit={values => {
            updateConnection(
              editingConnectionSetMember,
              {
                name: values.name,
                connection_info: generatePostgresRequestPayload({
                  driver: 'postgres',
                  values,
                }).details.configuration.connection_info,
              } as ConnectionSet,
              {
                onSuccess: () => {
                  hasuraToast({
                    type: 'success',
                    title: 'Connection updated',
                  });
                },
                onError: e => {
                  hasuraToast({
                    type: 'error',
                    title: 'Failed to update connection',
                    message: e.message,
                  });
                },
              }
            );

            setEditingConnectionSetMember(undefined);
          }}
          onClose={() => setEditingConnectionSetMember(undefined)}
        />
      )}
      <SimpleForm
        onSubmit={values => {
          updateConnectionTemplate(values.connection_template, {
            onSuccess: () => {
              hasuraToast({
                type: 'success',
                title: 'Connection template updated',
              });
            },
            onError: e => {
              hasuraToast({
                type: 'error',
                title: 'Failed to update connection template',
                message: e.message,
              });
            },
          });
        }}
        schema={schema}
        options={{
          defaultValues: {
            connection_template: connectionTemplate ?? undefined,
          },
        }}
      >
        <DynamicDBRoutingForm
          connectionSetMembers={connectionSetMembers}
          onAddConnection={() => setIsModalOpen(true)}
          onEditConnection={connectionName => {
            setEditingConnectionSetMember(connectionName);
          }}
          onRemoveConnection={connectionName => {
            removeConnection(connectionName, {
              onSuccess: () => {
                hasuraToast({
                  type: 'success',
                  title: 'Connection removed',
                });
              },
              onError: e => {
                hasuraToast({
                  type: 'error',
                  title: 'Failed to remove connection',
                  message: e.message,
                });
              },
            });
          }}
          isLoading={isLoading || isMetadaLoading}
          connectionTemplate={connectionTemplate}
        />
      </SimpleForm>
    </>
  );
};
