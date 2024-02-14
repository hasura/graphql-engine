import { useFieldArray, useFormContext } from 'react-hook-form';
import { Button } from '../../../../../new-components/Button';
import { CardedTable } from '../../../../../new-components/CardedTable';
import { useState } from 'react';
import { ConnectionInfoSchema } from '../schema';
import { FaEdit, FaPlus, FaTrash } from 'react-icons/fa';
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
  const { watch, setValue, trigger } =
    useFormContext<Record<string, ConnectionInfoSchema[]>>();

  const [mode, setMode] = useState<'idle' | 'add' | 'edit'>('idle');
  const readReplicas = watch(name);

  const [activeRow, setActiveRow] = useState<number>();

  return (
    <div className="my-2">
      {!fields?.length ? (
        <IndicatorCard status="info">No read replicas added.</IndicatorCard>
      ) : (
        <CardedTable
          columns={['No', 'Read Replica', null]}
          data={(readReplicas ?? []).map((field, i) => [
            i + 1,
            <div>{getDatabaseConnectionDisplayName(field.databaseUrl)}</div>,
            <div className="flex gap-3 justify-end">
              <Button
                size="sm"
                icon={<FaEdit />}
                onClick={() => {
                  setActiveRow(i);
                  setMode('edit');
                }}
              />
              <Button
                size="sm"
                icon={<FaTrash />}
                onClick={() => {
                  setValue(
                    name,
                    readReplicas.filter((_, index) => index !== i)
                  );
                }}
              />
            </div>,
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
            setActiveRow(readReplicas?.length ?? 0);
          }}
          mode="primary"
          icon={<FaPlus />}
        >
          Add New Read Replica
        </Button>
      )}

      {(mode === 'add' || mode === 'edit') && (
        <Dialog
          hasBackdrop
          title={mode === 'edit' ? 'Edit Read Replica' : 'Add Read Replica'}
          onClose={() => {
            setMode('idle');
          }}
          titleTooltip="Optional list of read replica configuration"
          size="xxxl"
        >
          <div className="p-4">
            <div className="bg-white border border-hasGray-300 rounded-md shadow-sm overflow-hidden p-4">
              <DatabaseUrl
                name={`${name}.${activeRow}.databaseUrl`}
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
                <PoolSettings name={`${name}.${activeRow}.poolSettings`} />
                <IsolationLevel name={`${name}.${activeRow}.isolationLevel`} />
                <UsePreparedStatements
                  name={`${name}.${activeRow}.usePreparedStatements`}
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
                    <SslSettings name={`${name}.${activeRow}.sslSettings`} />
                  </Collapsible>
                )}
              </Collapsible>
            </div>
            <Button
              onClick={async () => {
                // validate the current open read replica state before closing.
                const result = await trigger(`${name}.${activeRow}`);

                if (result) {
                  setMode('idle');
                  setActiveRow(undefined);
                }
              }}
              mode="primary"
              className="my-2"
            >
              {mode === 'edit' ? 'Edit Read Replica' : 'Add Read Replica'}
            </Button>
          </div>
        </Dialog>
      )}
    </div>
  );
};
