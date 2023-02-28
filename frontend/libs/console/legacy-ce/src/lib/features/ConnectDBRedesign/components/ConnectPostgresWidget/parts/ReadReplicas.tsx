import { useFieldArray, useFormContext } from 'react-hook-form';
import { Button } from '../../../../../new-components/Button';
import { CardedTable } from '../../../../../new-components/CardedTable';
import { useState } from 'react';
import { ConnectionInfoSchema } from '../schema';
import { FaPlus, FaTrash } from 'react-icons/fa';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import {
  areSSLSettingsEnabled,
  getDatabaseConnectionDisplayName,
} from '../utils/helpers';
import { DatabaseUrl } from './DatabaseUrl';
import { PoolSettings } from './PoolSettings';
import { IsolationLevel } from './IsolationLevel';
import { UsePreparedStatements } from './UsePreparedStatements';
import { SslSettings } from './SslSettings';
import { Dialog } from '../../../../../new-components/Dialog';
import { Collapsible } from '../../../../../new-components/Collapsible';

export const ReadReplicas = ({
  name,
  hideOptions,
}: {
  name: string;
  hideOptions: string[];
}) => {
  const { fields, append } = useFieldArray<
    Record<string, ConnectionInfoSchema[]>
  >({
    name,
  });
  const { watch, setValue } =
    useFormContext<Record<string, ConnectionInfoSchema[]>>();

  const [mode, setMode] = useState<'idle' | 'add'>('idle');
  const readReplicas = watch(name);

  return (
    <div className="my-2">
      {!fields?.length ? (
        <IndicatorCard status="info">No read replicas added.</IndicatorCard>
      ) : (
        <CardedTable
          columns={['No', 'Read Replica', null]}
          data={(fields ?? []).map((field, i) => [
            i + 1,
            <div>{getDatabaseConnectionDisplayName(field.databaseUrl)}</div>,
            <Button
              size="sm"
              icon={<FaTrash />}
              mode="destructive"
              onClick={() => {
                setValue(
                  name,
                  readReplicas.filter((_, index) => index !== i)
                );
              }}
            />,
          ])}
          showActionCell
        />
      )}

      {mode === 'idle' && (
        <Button
          type="button"
          onClick={() => {
            setMode('add');
            append({
              databaseUrl: { connectionType: 'databaseUrl', url: '' },
            });
          }}
          mode="primary"
          icon={<FaPlus />}
        >
          Add New Read Replica
        </Button>
      )}

      {mode === 'add' && (
        <Dialog
          hasBackdrop
          title="Add Read Replica"
          onClose={() => {
            setMode('idle');
          }}
          titleTooltip="Optional list of read replica configuration"
          size="xxxl"
        >
          <div className="p-4">
            <div className="bg-white border border-hasGray-300 rounded-md shadow-sm overflow-hidden p-4">
              <DatabaseUrl
                name={`${name}.${fields?.length - 1}.databaseUrl`}
                hideOptions={hideOptions}
              />
            </div>

            <div className="bg-white border border-hasGray-300 rounded-md shadow-sm overflow-hidden p-4 mt-sm">
              <Collapsible
                triggerChildren={
                  <div className="font-semibold text-muted">
                    Advanced Settings
                  </div>
                }
              >
                <PoolSettings
                  name={`${name}.${fields?.length - 1}.poolSettings`}
                />
                <IsolationLevel
                  name={`${name}.${fields?.length - 1}.isolationLevel`}
                />
                <UsePreparedStatements
                  name={`${name}.${fields?.length - 1}.usePreparedStatements`}
                />
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
                      name={`${name}.${fields?.length - 1}.sslSettings`}
                    />
                  </Collapsible>
                )}
              </Collapsible>
            </div>
            <Button
              onClick={() => {
                setMode('idle');
                setValue(
                  `${name}.${fields?.length - 1}`,
                  fields[fields?.length - 1]
                );
              }}
              mode="primary"
              className="my-2"
            >
              Add Read Replica
            </Button>
          </div>
        </Dialog>
      )}
    </div>
  );
};
