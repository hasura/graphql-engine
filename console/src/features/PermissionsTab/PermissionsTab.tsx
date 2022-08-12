import React from 'react';
import { useTableMachine, PermissionsTable } from '../PermissionsTable';
import { PermissionsForm, BulkDelete } from '../PermissionsForm';
import { AccessType } from '../PermissionsForm/types';
import { DataLeaf } from './types/types';

export interface PermissionsTabProps {
  dataLeaf: DataLeaf;
  tableType: 'table' | 'view';
}

export const PermissionsTab: React.FC<PermissionsTabProps> = ({ dataLeaf }) => {
  const machine = useTableMachine();
  const [state, send] = machine;

  return (
    <div className="p-4">
      <div className="grid gap-4">
        <PermissionsTable dataLeaf={dataLeaf} machine={machine} />

        {state.value === 'bulkOpen' &&
          !!state.context.bulkSelections.length && (
            <BulkDelete
              roles={state.context.bulkSelections}
              dataLeaf={dataLeaf}
              handleClose={() => send('CLOSE')}
            />
          )}

        {state.value === 'formOpen' && (
          <PermissionsForm
            dataLeaf={dataLeaf}
            roleName={state.context.selectedForm.roleName || ''}
            accessType={state.context.selectedForm.accessType as AccessType}
            queryType={state.context.selectedForm.queryType || 'insert'}
            handleClose={() => send('CLOSE')}
          />
        )}
      </div>
    </div>
  );
};

export default PermissionsTab;
