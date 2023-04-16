import React from 'react';
import z from 'zod';

import { useConsoleForm } from '../../../../../../new-components/Form';
import { ConnectPostgresModal } from './ConnectPostgresModal';
import { DynamicDBRoutingForm } from './DynamicDBRoutingForm';
import { generatePostgresRequestPayload } from '../../utils/generateRequests';
import { adaptPostgresConnectionInfo } from '../../utils/adaptResponse';
import { useDynamicDbRouting } from './hooks/useDynamicDbRouting';
import { ConnectionSet } from '../../../../../../metadata/types';
import { PostgresConfiguration } from '../../../../../hasura-metadata-types';
import { hasuraToast } from '../../../../../../new-components/Toasts';
import { ValidateModal } from './ValidateModal';

export const schema = z.object({
  connection_template: z.string().optional(),
  validation: z
    .object({
      connection_template: z.string().optional(),
      headers: z
        .array(
          z.object({
            checked: z.boolean(),
            key: z.string(),
            value: z.string(),
          })
        )
        .optional(),
      session_variables: z
        .array(
          z.object({
            checked: z.boolean(),
            key: z.string(),
            value: z.string(),
          })
        )
        .optional(),
      operation_type: z.string(),
      operation_name: z.string().optional(),
    })
    .optional(),
});

interface DynamicDBRoutingProps {
  sourceName: string;
}

const loadFormData = (): z.infer<typeof schema>['validation'] => {
  const data = localStorage.getItem('dynamic-db-routing-context');
  return data ? JSON.parse(data) : null;
};

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
  const [isValidateModalOpen, setIsValidateModalOpen] = React.useState(false);

  const {
    Form,
    methods: { watch, setValue },
  } = useConsoleForm({
    schema,
    options: {
      defaultValues: {
        connection_template: connectionTemplate ?? undefined,
        validation: loadFormData() ?? {
          connection_template: connectionTemplate ?? undefined,
          headers: [
            {
              checked: true,
              key: '',
              value: '',
            },
          ],
          session_variables: [
            {
              checked: true,
              key: 'x-hasura-role',
              value: 'user',
            },
          ],
          operation_type: 'query',
        },
      },
    },
  });

  const formTemplate = watch('connection_template');
  const modalTemplate = watch('validation.connection_template');

  // keep form template and modal template in sync
  React.useEffect(() => {
    if (modalTemplate && modalTemplate && formTemplate !== modalTemplate) {
      setValue('connection_template', modalTemplate);
    }
  }, [modalTemplate]);

  React.useEffect(() => {
    if (formTemplate && formTemplate && modalTemplate !== formTemplate) {
      setValue('validation.connection_template', formTemplate);
    }
  }, [formTemplate]);

  React.useEffect(() => {
    if (connectionTemplate) {
      setValue('connection_template', connectionTemplate);
    }
  }, [connectionTemplate]);

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
      <Form
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
      >
        {isValidateModalOpen && (
          <ValidateModal
            sourceName={props.sourceName}
            onClose={() => setIsValidateModalOpen(false)}
          />
        )}
        <DynamicDBRoutingForm
          onOpenValidate={() => setIsValidateModalOpen(true)}
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
      </Form>
    </>
  );
};
