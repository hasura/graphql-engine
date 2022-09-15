import React from 'react';
// eslint-disable-next-line import/first
import { Table } from '@/features/DataSource';
import {
  FaArrowRight,
  FaColumns,
  FaDatabase,
  FaEdit,
  FaFont,
  FaPlug,
  FaTable,
  FaTrash,
} from 'react-icons/fa';
import { Button } from '@/new-components/Button';
import { CardedTable } from '@/new-components/CardedTable';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { Relationship } from './types';
import Legends from './Legends';
import { useListAllRelationshipsFromMetadata } from './hooks/useListAllRelationshipsFromMetadata';

export const columns = ['NAME', 'SOURCE', 'TYPE', 'RELATIONSHIP', null];

const getTableDisplayName = (table: Table): string => {
  if (!table) return 'Empty Object';

  if (typeof table === 'string') return table;

  if (typeof table === 'object' && 'name' in table)
    return (table as { name: string }).name;

  return JSON.stringify(table);
};

function TargetName(props: { relationship: Relationship }) {
  const { relationship } = props;
  if (relationship.type === 'toRemoteSchema')
    return (
      <>
        <FaPlug /> <span>{relationship.mapping.to.remoteSchema}</span>
      </>
    );

  return (
    <>
      <FaDatabase /> <span>{relationship.mapping.to.source}</span>
    </>
  );
}

const RelationshipMapping = ({
  relationship,
}: {
  relationship: Relationship;
}) => {
  return (
    <div className="flex items-center gap-6">
      <div className="flex items-center gap-2">
        <FaTable />
        <span>{getTableDisplayName(relationship.mapping.from.table)}</span>
        /
        <FaColumns /> {relationship.mapping.from.columns.join(',')}
      </div>
      <FaArrowRight />

      <div className="flex items-center gap-2">
        {relationship.type === 'toRemoteSchema' ? (
          <>
            <FaPlug />
            <span>{relationship.mapping.to.remoteSchema}</span> /
            <FaFont /> {relationship.mapping.to.fields.join(',')}
          </>
        ) : (
          <>
            <FaTable />
            <span>{getTableDisplayName(relationship.mapping.to.table)}</span> /
            <FaColumns />
            {!relationship.mapping.to.columns.length
              ? '[could not find target columns]'
              : relationship.mapping.to.columns.join(',')}
          </>
        )}
      </div>
    </div>
  );
};

export interface DatabaseRelationshipsTableProps {
  dataSourceName: string;
  table: Table;
  onEditRow: (props: {
    dataSourceName: string;
    table: Table;
    relationship: Relationship;
  }) => void;
  onDeleteRow: (props: {
    dataSourceName: string;
    table: Table;
    relationship: Relationship;
  }) => void;
}

export const DatabaseRelationshipsTable = ({
  dataSourceName,
  table,
  onEditRow,
  onDeleteRow,
}: DatabaseRelationshipsTableProps) => {
  const {
    data: relationships,
    isLoading,
    isError,
  } = useListAllRelationshipsFromMetadata(dataSourceName, table);
  if (isError && !isLoading)
    return (
      <IndicatorCard
        status="negative"
        headline="Error while fetching relationships"
      />
    );

  if (!relationships && isLoading)
    return <IndicatorCard status="info" headline="Fetching relationships..." />;

  if (!relationships || relationships.length === 0)
    return <IndicatorCard status="info" headline="No relationships found" />;

  return (
    <>
      <CardedTable.Table>
        <CardedTable.Header columns={columns} />
        <CardedTable.TableBody>
          {relationships.map(relationship => (
            <CardedTable.TableBodyRow key={relationship.name}>
              <CardedTable.TableBodyCell>
                <Button
                  onClick={() =>
                    onEditRow({
                      dataSourceName,
                      table,
                      relationship,
                    })
                  }
                >
                  {relationship.name}
                </Button>
              </CardedTable.TableBodyCell>

              <CardedTable.TableBodyCell>
                <div className="flex items-center gap-2">
                  <TargetName relationship={relationship} />
                </div>
              </CardedTable.TableBodyCell>

              <CardedTable.TableBodyCell>
                {relationship.relationship_type}
              </CardedTable.TableBodyCell>

              <CardedTable.TableBodyCell>
                <RelationshipMapping relationship={relationship} />
              </CardedTable.TableBodyCell>

              <CardedTable.TableBodyActionCell>
                <div className="flex items-center justify-end whitespace-nowrap text-right">
                  <button
                    onClick={() =>
                      onEditRow({
                        dataSourceName,
                        table,
                        relationship,
                      })
                    }
                    className="flex px-2 py-0.5 items-center font-semibold rounded text-secondary mr-0.5 hover:bg-indigo-50 focus:bg-indigo-100"
                  >
                    <FaEdit className="fill-current mr-1" />
                    Edit
                  </button>
                  <button
                    onClick={() =>
                      onDeleteRow({
                        dataSourceName,
                        table,
                        relationship,
                      })
                    }
                    className="flex px-2 py-0.5 items-center font-semibold rounded text-red-700 hover:bg-red-50 focus:bg-red-100"
                  >
                    <FaTrash className="fill-current mr-1" />
                    Remove
                  </button>
                </div>
              </CardedTable.TableBodyActionCell>
            </CardedTable.TableBodyRow>
          ))}
        </CardedTable.TableBody>
      </CardedTable.Table>
      <Legends />
    </>
  );
};
