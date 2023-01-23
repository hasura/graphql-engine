import React from 'react';

import { useTableMachine, PermissionsTable } from '../PermissionsTable';
import { BulkDelete, PermissionsForm } from '../PermissionsForm';

import { AccessType } from '../types';

export interface PermissionsTabProps {
  dataSourceName: string;
  table: unknown;
}

export const PermissionsTab: React.FC<PermissionsTabProps> = ({
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
              dataSourceName={dataSourceName}
              table={table}
              handleClose={() => send('CLOSE')}
            />
          )}

        {state.value === 'formOpen' && (
          <PermissionsForm
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
