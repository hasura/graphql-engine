import React from 'react';

import { useTableMachine, PermissionsTable } from '../PermissionsTable';
import { BulkDelete, PermissionsForm } from '../PermissionsForm';
import { AccessType, MetadataTable, Source } from '@hasura/shared/types';

export interface PermissionsTabProps {
  source: Source;
  table: MetadataTable;
}

export const PermissionsTab: React.FC<PermissionsTabProps> = ({
  source,
  table,
}) => {
  const machine = useTableMachine();
  const [state, send] = machine;

  return (
    <div className="p-4">
      <div className="grid gap-4">
        <PermissionsTable
          source={source}
          table={table.table}
          machine={machine}
        />

        {state.value === 'bulkOpen' &&
          !!state.context.bulkSelections.length && (
            <BulkDelete
              roles={state.context.bulkSelections}
              dataSourceName={source.name}
              table={table.table}
              handleClose={() => send('CLOSE')}
            />
          )}

        {state.value === 'formOpen' && (
          <PermissionsForm
            source={source}
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
