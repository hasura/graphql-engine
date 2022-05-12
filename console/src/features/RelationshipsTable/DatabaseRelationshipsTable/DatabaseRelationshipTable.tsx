import React from 'react';

import { FaArrowRight } from 'react-icons/fa';
import { CardedTable } from '@/new-components/CardedTable';
import {
  useDbToRemoteSchemaRelationships,
  useDbToRemoteDbRelationships,
} from '@/features/MetadataAPI';
import { DataTarget } from '@/features/Datasources';
import {
  ModifyActions,
  TableRowIcon,
  SourceRelationshipCell,
  DestinationRelationshipCell,
} from './components';

import { getRemoteFieldPath } from '../utils';
import { RowData } from './types';

export const columns = ['NAME', 'SOURCE', 'TYPE', 'RELATIONSHIP', null];

// fetch the data from the relevant hooks
const useTableData = (target: DataTarget) => {
  const {
    data: dbToRsData,
    isLoading: dbToRsIsLoading,
    isError: dbToRsIsError,
  } = useDbToRemoteSchemaRelationships(target);

  const {
    data: dbToDbData,
    isLoading: dbToDbIsLoading,
    isError: dbToDbIsError,
  } = useDbToRemoteDbRelationships(target);

  // convert it into a consistent useful format for the table
  const data = React.useMemo(() => {
    // other relationships will be added here
    const dbToRs: RowData[] =
      dbToRsData?.map(relationship => ({
        fromType: 'database',
        toType: 'remote_schema',
        name: relationship?.relationshipName,
        source: relationship?.target?.table,
        destination: relationship.remoteSchemaName,
        type: 'Remote Schema',
        fieldsFrom: relationship?.lhs_fields || [],
        fieldsTo: getRemoteFieldPath(relationship?.remote_field),
        relationship,
      })) || [];

    const dbToDb: RowData[] =
      dbToDbData?.map(relationship => ({
        fromType: 'database',
        toType: 'database',
        name: relationship?.relationshipName,
        source: relationship?.target?.table,
        destination: relationship.remoteDbName,
        type: relationship.relationshipType,
        fieldsFrom: Object.keys(relationship.fieldMapping) || [],
        fieldsTo: Object.values(relationship.fieldMapping) || [],
        relationship,
      })) || [];

    return [...dbToRs, ...dbToDb];
  }, [dbToRsData, dbToDbData]);

  const isLoading = dbToRsIsLoading || dbToDbIsLoading;
  const isError = dbToRsIsError || dbToDbIsError;

  return { data, isLoading, isError };
};

export interface DatabaseRelationshipsTableProps {
  target: DataTarget;
  onClick: (input: OnClickHandlerArgs) => void;
}

export interface OnClickHandlerArgs {
  type: 'add' | 'edit' | 'delete';
  row?: RowData;
}

export const DatabaseRelationshipsTable = ({
  target,
  onClick,
}: DatabaseRelationshipsTableProps) => {
  const { data } = useTableData(target);

  return (
    <CardedTable.Table>
      <CardedTable.Header columns={columns} />
      <CardedTable.TableBody>
        {data.map(row => (
          <CardedTable.TableBodyRow key={row.name}>
            <CardedTable.TableBodyCell>
              <button
                className="text-secondary cursor-pointer"
                onClick={() => onClick({ type: 'add', row })}
              >
                {row.name}
              </button>
            </CardedTable.TableBodyCell>
            <CardedTable.TableBodyCell>
              <div className="flex items-center gap-2">
                <TableRowIcon type={row.fromType} />
                <span>{row.source}</span>
              </div>
            </CardedTable.TableBodyCell>
            <CardedTable.TableBodyCell>{row.type}</CardedTable.TableBodyCell>
            <CardedTable.TableBodyCell>
              <SourceRelationshipCell row={row} />
            </CardedTable.TableBodyCell>
            <CardedTable.TableBodyCell>
              <FaArrowRight className="fill-current text-sm text-muted" />
            </CardedTable.TableBodyCell>
            <CardedTable.TableBodyCell>
              <DestinationRelationshipCell row={row} />
            </CardedTable.TableBodyCell>
            <CardedTable.TableBodyActionCell>
              <ModifyActions
                onEdit={() => onClick({ type: 'edit', row })}
                onDelete={() => onClick({ type: 'delete', row })}
              />
            </CardedTable.TableBodyActionCell>
          </CardedTable.TableBodyRow>
        ))}
      </CardedTable.TableBody>
    </CardedTable.Table>
  );
};
