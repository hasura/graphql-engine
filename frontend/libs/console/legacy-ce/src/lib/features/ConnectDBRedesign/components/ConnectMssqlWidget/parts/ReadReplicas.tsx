import { useFieldArray, useFormContext } from 'react-hook-form';
import {
  Button,
  CardedTable,
  IndicatorCard,
  Dialog,
  Collapsible,
  Text,
  IconButton,
} from '@hasura/shared/ui';
import { ConnectionString } from './ConnectionString';
import { useState } from 'react';
import { ConnectionInfoSchema } from '../schema';
import { FaEdit, FaPlus, FaTrash } from 'react-icons/fa';
import { PoolSettings } from './PoolSettings';
import { Flex } from '@radix-ui/themes';

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
    <div>
      {!fields?.length ? (
        <IndicatorCard status="info">No read replicas added.</IndicatorCard>
      ) : (
        <CardedTable
          columns={['No', 'Read Replica', null]}
          data={(readReplicas ?? []).map((field, i) => [
            i + 1,
            <div key={`url-${i}`}>
              {field.connectionString.connectionType === 'databaseUrl'
                ? field.connectionString.url
                : field.connectionString.envVar}
            </div>,
            <Flex gap="3" align="center" justify="end" key={`actions-${i}`}>
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
                color="red"
                variant="ghost"
                onClick={() => {
                  setValue(
                    name,
                    readReplicas.filter((_, index) => index !== i),
                  );
                }}
              >
                <FaTrash />
              </IconButton>
            </Flex>,
          ])}
        />
      )}

      {mode === 'idle' && (
        <div className="mt-2">
          <Button
            type="button"
            mode="default"
            size="1"
            onClick={() => {
              setMode('add');
              append({
                connectionString: { connectionType: 'databaseUrl', url: '' },
              });
              setActiveRow(readReplicas?.length ?? 0);
            }}
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
            <ConnectionString name={`${name}.${activeRow}.connectionString`} />

            <div className="my-4">
              <Collapsible
                triggerChildren={<Text weight="bold">Advanced Settings</Text>}
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
