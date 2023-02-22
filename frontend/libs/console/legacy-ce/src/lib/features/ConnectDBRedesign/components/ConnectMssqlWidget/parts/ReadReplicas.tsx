import { useFieldArray, useFormContext } from 'react-hook-form';
import { Button } from '../../../../../new-components/Button';
import { CardedTable } from '../../../../../new-components/CardedTable';
import { ConnectionInfo } from './ConnectionInfo';
import { useState } from 'react';
import { ConnectionInfoSchema } from '../schema';
import { FaPlus, FaTrash } from 'react-icons/fa';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';

export const ReadReplicas = ({ name }: { name: string }) => {
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
          data={(fields ?? []).map((x, i) => [
            i + 1,
            <div>
              {x.connectionString.connectionType === 'databaseUrl'
                ? x.connectionString.url
                : x.connectionString.envVar}
            </div>,
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
              connectionString: { connectionType: 'databaseUrl', url: '' },
            });
          }}
          mode="primary"
          icon={<FaPlus />}
        >
          Add New Read Replica
        </Button>
      )}

      {mode === 'add' && (
        <div>
          <ConnectionInfo name={`${name}.${fields?.length - 1}`} />
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
      )}
    </div>
  );
};
