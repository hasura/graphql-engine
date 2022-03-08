import React from 'react';
import { RouteComponentProps } from 'react-router';

import { useSingleTable } from '@/hooks/useTables';
import { useAppSelector } from '@/store';
import { currentDriver } from '@/dataSources';

import TableHeader from '../../components/Services/Data/TableCommon/TableHeader';
import { RightContainer } from '../../components/Common/Layout/RightContainer';

import { PermissionsTable, useTableMachine } from '../PermissionsTable';
import { PermissionsForm, BulkDelete } from '../PermissionsForm';
import { AccessType } from '../PermissionsForm/types';

interface PermissionRouteParams {
  schema: string;
  table: string;
}

export interface PermissionsTabProps
  extends RouteComponentProps<{ tab: string }, PermissionRouteParams> {
  tableType: 'table' | 'view';
}

export const PermissionsTab: React.FC<PermissionsTabProps> = ({
  params: { table, schema },
}) => {
  const source: string = useAppSelector(
    state => state.tables.currentDataSource
  );

  const currentSchema = useSingleTable({
    source,
    driver: currentDriver,
    table: { name: table, schema },
  });

  const machine = useTableMachine();
  const [state, send] = machine;

  if (!currentSchema) {
    return <div>Loading...</div>;
  }

  return (
    <RightContainer>
      <TableHeader
        dispatch={() => {}}
        table={currentSchema}
        source={source}
        tabName="permissions"
        migrationMode={false}
        readOnlyMode={false}
        count={null}
        isCountEstimated
      />
      <div className="p-4">
        <div className="grid gap-4">
          <PermissionsTable
            schemaName={schema}
            tableName={table}
            machine={machine}
          />

          {state.value === 'bulkOpen' &&
            !!state.context.bulkSelections.length && (
              <BulkDelete
                roles={state.context.bulkSelections}
                tableName={table}
                schemaName={schema}
                handleClose={() => send('CLOSE')}
              />
            )}

          {state.value === 'formOpen' && (
            <PermissionsForm
              schemaName={schema}
              tableName={table}
              roleName={state.context.selectedForm.roleName || ''}
              accessType={state.context.selectedForm.accessType as AccessType}
              queryType={state.context.selectedForm.queryType || 'insert'}
              handleClose={() => send('CLOSE')}
            />
          )}
        </div>
      </div>
    </RightContainer>
  );
};

export default PermissionsTab;
