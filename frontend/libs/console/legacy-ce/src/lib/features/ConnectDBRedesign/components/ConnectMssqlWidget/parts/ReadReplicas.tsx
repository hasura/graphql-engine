import { useFieldArray, useFormContext } from 'react-hook-form';
import { Button } from '../../../../../new-components/Button';
import { CardedTable } from '../../../../../new-components/CardedTable';
import { ConnectionString } from './ConnectionString';
import { useState } from 'react';
import { ConnectionInfoSchema } from '../schema';
import { FaEdit, FaPlus, FaTrash } from 'react-icons/fa';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import { Dialog } from '../../../../../new-components/Dialog';
import { Collapsible } from '../../../../../new-components/Collapsible';
import { PoolSettings } from './PoolSettings';

// export const ReadReplicas = ({ name }: { name: string }) => {
//   const { fields, append } = useFieldArray<
//     Record<string, ConnectionInfoSchema[]>
//   >({
//     name,
//   });
//   const { watch, setValue } =
//     useFormContext<Record<string, ConnectionInfoSchema[]>>();

//   const [mode, setMode] = useState<'idle' | 'add'>('idle');
//   const readReplicas = watch(name);

//   return (
//     <div className="my-2">
//       {!fields?.length ? (
//         <IndicatorCard status="info">No read replicas added.</IndicatorCard>
//       ) : (
//         <CardedTable
//           columns={['No', 'Read Replica', null]}
//           data={(fields ?? []).map((x, i) => [
//             i + 1,
//             <div>
//               {x.connectionString.connectionType === 'databaseUrl'
//                 ? x.connectionString.url
//                 : x.connectionString.envVar}
//             </div>,
//             <Button
//               size="sm"
//               icon={<FaTrash />}
//               mode="destructive"
//               onClick={() => {
//                 setValue(
//                   name,
//                   readReplicas.filter((_, index) => index !== i)
//                 );
//               }}
//             />,
//           ])}
//           showActionCell
//         />
//       )}

//       {mode === 'idle' && (
//         <Button
//           type="button"
//           onClick={() => {
//             setMode('add');
//             append({
//               connectionString: { connectionType: 'databaseUrl', url: '' },
//             });
//           }}
//           mode="primary"
//           icon={<FaPlus />}
//         >
//           Add New Read Replica
//         </Button>
//       )}

//       {mode === 'add' && (
//         <div>
//           <ConnectionInfo name={`${name}.${fields?.length - 1}`} />
//           <Button
//             onClick={() => {
//               setMode('idle');
//               setValue(
//                 `${name}.${fields?.length - 1}`,
//                 fields[fields?.length - 1]
//               );
//             }}
//             mode="primary"
//             className="my-2"
//           >
//             Add Read Replica
//           </Button>
//         </div>
//       )}
//     </div>
//   );
// };

export const ReadReplicas = ({ name }: { name: string }) => {
  const { fields, append } = useFieldArray<
    Record<string, ConnectionInfoSchema[]>
  >({
    name,
  });
  const { watch, setValue } =
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
            <div>
              {field.connectionString.connectionType === 'databaseUrl'
                ? field.connectionString.url
                : field.connectionString.envVar}
            </div>,
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
              connectionString: { connectionType: 'databaseUrl', url: '' },
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
              <ConnectionString
                name={`${name}.${activeRow}.connectionString`}
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
              </Collapsible>
            </div>
            <Button
              onClick={() => {
                setMode('idle');
                setActiveRow(undefined);
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
