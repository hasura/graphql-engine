import React from 'react';
import { useTableMachine, PermissionsTable } from '../PermissionsTable';
import { BulkDelete } from '../PermissionsForm';
import { PermissionsForm } from '../PermissionsForm/PermissionsForm';
import { AccessType } from '../PermissionsForm/types';

export interface PermissionsTabProps {
  currentSource: string;
  dataSourceName: string;
  table: unknown;
}

export const PermissionsTab: React.FC<PermissionsTabProps> = ({
  currentSource,
  dataSourceName,
  table,
}) => {
  const machine = useTableMachine();
  const [state, send] = machine;

  return (
    <div className="p-4">
      <div className="grid gap-4">
        <PermissionsTable
          dataSourceName={dataSourceName}
          table={table}
          machine={machine}
        />

        {state.value === 'bulkOpen' &&
          !!state.context.bulkSelections.length && (
            <BulkDelete
              roles={state.context.bulkSelections}
              currentSource={currentSource}
              dataSourceName={dataSourceName}
              table={table}
              handleClose={() => send('CLOSE')}
            />
          )}

        {state.value === 'formOpen' && (
          <PermissionsForm
            currentSource={currentSource}
            dataSourceName={dataSourceName}
            table={table}
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
