import React from 'react';
import z from 'zod';

import { Collapsible } from '../../../../../../new-components/Collapsible';
import { Dialog } from '../../../../../../new-components/Dialog';
import { schema } from '../../schema';

import {
  InputField,
  useConsoleForm,
} from '../../../../../../new-components/Form';
import { areSSLSettingsEnabled } from '../../utils/helpers';
import { DatabaseUrl } from '../DatabaseUrl';
import { IsolationLevel } from '../IsolationLevel';
import { PoolSettings } from '../PoolSettings';
import { SslSettings } from '../SslSettings';
import { UsePreparedStatements } from '../UsePreparedStatements';

interface ConnectPostgresModalProps {
  alreadyUseNames?: string[];
  defaultValues?: z.infer<typeof schema>;
  onClose: () => void;
  onSubmit: (values: z.infer<typeof schema>) => void;
}

export const ConnectPostgresModal = (props: ConnectPostgresModalProps) => {
  const { onClose, onSubmit, defaultValues, alreadyUseNames } = props;

  const {
    Form,
    methods: { setError },
  } = useConsoleForm({
    schema,
    options: {
      defaultValues: defaultValues ?? {
        configuration: {
          connectionInfo: {
            databaseUrl: {
              connectionType: 'databaseUrl',
            },
          },
        },
      },
    },
  });

  return (
    <Form
      onSubmit={(values: z.infer<typeof schema>) => {
        if (alreadyUseNames?.includes(values.name)) {
          setError('name', {
            type: 'manual',
            message: 'This name is already in use',
          });
        } else {
          onSubmit(values);
        }
      }}
    >
      <Dialog
        size="lg"
        hasBackdrop
        title={defaultValues ? 'Edit Connection' : 'Add Connection'}
        description={`${
          defaultValues ? 'Edit connection' : 'Add connections'
        } which will be available to be referenced in your dynamic connection template.`}
        onClose={onClose}
      >
        <>
          <div className="px-6 mb-8">
            <InputField
              name="name"
              label="Connection name"
              placeholder="Connection name"
            />

            <div>
              <DatabaseUrl
                name="configuration.connectionInfo.databaseUrl"
                hideOptions={[]}
              />
            </div>

            <div className="mt-sm">
              <Collapsible
                triggerChildren={
                  <div className="font-semibold text-muted">
                    Advanced Settings
                  </div>
                }
              >
                <PoolSettings
                  name={`configuration.connectionInfo.poolSettings`}
                />
                <IsolationLevel name={`configuration.connectionInfo`} />
                <UsePreparedStatements name={`configuration.connectionInfo`} />
                {areSSLSettingsEnabled() && (
                  <Collapsible
                    triggerChildren={
                      <div className="font-semibold text-muted">
                        SSL Certificates Settings
                        <span className="px-1.5 italic font-light">
                          (Certificates will be loaded from{' '}
                          <a href="https://hasura.io/docs/latest/graphql/cloud/projects/create.html#existing-database">
                            environment variables
                          </a>
                          )
                        </span>
                      </div>
                    }
                  >
                    <SslSettings
                      name={`configuration.connectionInfo.sslSettings`}
                    />
                  </Collapsible>
                )}
              </Collapsible>
            </div>
          </div>
          <Dialog.Footer
            callToDeny="Cancel"
            callToAction={
              defaultValues ? 'Update Connection' : 'Add Connection'
            }
            onClose={onClose}
            onSubmit={() => {}}
            onSubmitAnalyticsName="data-tab-dynamic-db-routing-add-connection-submit"
            onCancelAnalyticsName="data-tab-dynamic-db-routing-add-connection-cancel"
          />
        </>
      </Dialog>
    </Form>
  );
};
