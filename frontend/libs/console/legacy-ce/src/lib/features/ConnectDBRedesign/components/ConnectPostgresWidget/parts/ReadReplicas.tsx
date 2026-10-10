import { useFieldArray, useFormContext } from 'react-hook-form';
import {
  Button,
  CardedTable,
  IndicatorCard,
  Dialog,
  Collapsible,
  Card,
  Text,
  IconButton,
} from '@hasura/shared/ui';

import { useState } from 'react';
import { ConnectionInfoSchema } from '../schema';
import { FaEdit, FaPlus, FaTrash } from 'react-icons/fa';

import {
  areSSLSettingsEnabled,
  getDatabaseConnectionDisplayName,
} from '../utils/helpers';
import { DatabaseUrl } from './DatabaseUrl';
import { PoolSettings } from './PoolSettings';
import { IsolationLevel } from './IsolationLevel';
import { UsePreparedStatements } from './UsePreparedStatements';
import { SslSettings } from './SslSettings';

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
    <>
      <div className="mb-2">
        {!fields?.length ? (
          <IndicatorCard status="info">No read replicas added.</IndicatorCard>
        ) : (
          <CardedTable
            columns={['No', 'Read Replica', null]}
            data={(readReplicas ?? []).map((field, i) => [
              i + 1,
              <div key={`url-${i}`}>
                {getDatabaseConnectionDisplayName(field.databaseUrl)}
              </div>,
              <div key={`actions-${i}`} className="flex gap-3 justify-end">
                <IconButton
                  variant="ghost"
                  onClick={() => {
                    setActiveRow(i);
                    setMode('edit');
                  }}
                >
                  <FaEdit />
                </IconButton>
                <IconButton
                  variant="ghost"
                  color="red"
                  onClick={() => {
                    setValue(
                      name,
                      readReplicas.filter((_, index) => index !== i),
                    );
                  }}
                >
                  <FaTrash />
                </IconButton>
              </div>,
            ])}
          />
        )}
      </div>
      {mode === 'idle' && (
        <div className="mt-2">
          <Button
            type="button"
            onClick={() => {
              setMode('add');
              append({
                databaseUrl: { connectionType: 'databaseUrl', url: '' },
              });
              setActiveRow(readReplicas?.length ?? 0);
            }}
            mode="default"
            size="1"
            leftIcon={FaPlus}
          >
            Add New Read Replica
          </Button>
        </div>
      )}

      {(mode === 'add' || mode === 'edit') && (
        <Dialog
          title={mode === 'edit' ? 'Edit Read Replica' : 'Add Read Replica'}
          onClose={() => {
            setMode('idle');
          }}
          titleTooltip="Optional list of read replica configuration"
          size="xxxl"
        >
          <div>
            <Card size="2">
              <DatabaseUrl
                name={`${name}.${activeRow}.databaseUrl`}
                hideOptions={hideOptions}
              />
            </Card>

            <Card className="my-4" size="2">
              <Collapsible
                triggerChildren={
                  <Text weight="bold" className="cursor-pointer">
                    Advanced Settings
                  </Text>
                }
              >
                <PoolSettings name={`${name}.${activeRow}.poolSettings`} />
                <div className="pb-2">
                  <IsolationLevel
                    name={`${name}.${activeRow}.isolationLevel`}
                  />
                </div>
                <UsePreparedStatements
                  name={`${name}.${activeRow}.usePreparedStatements`}
                />
                {areSSLSettingsEnabled() && (
                  <SslSettings name={`${name}.${activeRow}.sslSettings`} />
                )}
              </Collapsible>
            </Card>
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
    </>
  );
};
