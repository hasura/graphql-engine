import React from 'react';

import { FaArrowRight } from 'react-icons/fa';
import { CardedTable } from '@/new-components/CardedTable';
import {
  useDbToRemoteSchemaRelationships,
  useDbToRemoteDbRelationships,
  useLocalRelationships,
} from '@/features/MetadataAPI';
import { DataTarget } from '@/features/Datasources';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import {
  ModifyActions,
  TableRowIcon,
  SourceRelationshipCell,
  DestinationRelationshipCell,
} from './components';

import { getRemoteFieldPath } from '../utils';
import { RowData } from './types';
import Legends from './Legends';

export const columns = ['NAME', 'TARGET', 'TYPE', 'RELATIONSHIP', null];

// fetch the data from the relevant hooks
const useTableData = (target: DataTarget) => {
  const {
    data: dbToRsData,
    isLoading: dbToRsIsLoading,
    isError: dbToRsIsError,
  } = useDbToRemoteSchemaRelationships(target);

  const {
    data: localrelationships,
    isLoading: localRelnsIsLoading,
  } = useLocalRelationships(target);

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
        fromType: 'table',
        toType: 'remote_schema',
        name: relationship?.relationshipName,
        reference: target.database,
        referenceTable: target.table,
        target: relationship.remoteSchemaName,
        type: 'Remote Schema',
        fieldsFrom: relationship?.lhs_fields || [],
        fieldsTo: getRemoteFieldPath(relationship?.remote_field),
        relationship,
      })) || [];

    const dbToDb: RowData[] =
      dbToDbData?.map(relationship => ({
        fromType: 'table',
        toType: 'database',
        name: relationship?.relationshipName,
        reference: target.database,
        referenceTable: target.table,
        target: relationship.remoteDbName,
        ...(relationship?.target?.table && {
          targetTable: relationship?.target?.table,
        }),
        type: relationship.relationshipType,
        fieldsFrom: Object.keys(relationship.fieldMapping) || [],
        fieldsTo: Object.values(relationship.fieldMapping) || [],
        relationship,
      })) || [];

    return [...localrelationships, ...dbToRs, ...dbToDb];
  }, [dbToRsData, dbToDbData, localrelationships, target]);

  const isLoading = dbToRsIsLoading || dbToDbIsLoading || localRelnsIsLoading;
  const isError = dbToRsIsError || dbToDbIsError;

  return { data, isLoading, isError };
};

export interface DatabaseRelationshipsTableProps {
  target: DataTarget;
  onClick: (input: OnClickHandlerArgs) => void;
}

export interface OnClickHandlerArgs {
  type: 'add' | 'edit' | 'delete' | 'close';
  row?: RowData;
}

export const DatabaseRelationshipsTable = ({
  target,
  onClick,
}: DatabaseRelationshipsTableProps) => {
  const { data, isLoading, isError } = useTableData(target);
  if (isError && !isLoading)
    return (
      <IndicatorCard
        status="negative"
        headline="Error fetching relationships"
      />
    );

  if (!data && isLoading)
    return <IndicatorCard status="info" headline="Fetching relationships..." />;

  if (!data || data?.length === 0)
    return <IndicatorCard status="info" headline="No relationships found" />;

  return (
    <>
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
                  <TableRowIcon
                    type={
                      row.toType === 'remote_schema'
                        ? 'remote_schema'
                        : 'database'
                    }
                  />
                  <span>{row.target}</span>
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
      <Legends />
    </>
  );
};
