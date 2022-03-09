import { TableEntry } from '@/metadata/types';
import { CardedTable } from '@/new-components/CardedTable';
import React, { ReactNode } from 'react';
import { FaArrowRight } from 'react-icons/fa';
import ModifyActions from './components/ModifyActions';
import NameColumnCell from './components/NameColumnCell';
import RelationshipDestinationCell from './components/RelationshipDestinationCell';
import SourceColumnCell from './components/SourceColumnCell';
import RelationshipSourceColumnCell from './components/RelationshipSourceCell';
import { RelationshipType } from './types';
import { getRemoteRelationType } from './utils';

export const columns = ['NAME', 'SOURCE', 'TYPE', 'RELATIONSHIP', null];

export interface RelationshipsTableProps {
  tableDefinition: TableEntry;
  onEdit: (relationship: RelationshipType) => void;
  onDelete: (relationship: RelationshipType) => void;
  onClick: (relationship: RelationshipType) => void;
}

export const RelationshipsTable = ({
  tableDefinition,
  onEdit,
  onDelete,
  onClick,
}: RelationshipsTableProps) => {
  let rowData: ReactNode[][] = [];

  if (tableDefinition?.object_relationships) {
    rowData = tableDefinition?.object_relationships?.map(i => [
      <NameColumnCell relationship={i} onClick={onClick} />,
      <SourceColumnCell type="local_object" name="Local Relationship" />,
      'Object',
      <RelationshipSourceColumnCell
        tableName={tableDefinition?.table?.name}
        relationship={i}
        sourceType="local_object"
      />,
      <FaArrowRight className="fill-current text-sm text-muted" />,
      <RelationshipDestinationCell
        relationship={i}
        sourceType="local_object"
      />,
      <ModifyActions onEdit={onEdit} onDelete={onDelete} relationship={i} />,
    ]);
  }

  if (tableDefinition?.array_relationships) {
    rowData = [
      ...rowData,
      ...tableDefinition?.array_relationships?.map(i => [
        <NameColumnCell relationship={i} onClick={onClick} />,
        <SourceColumnCell type="local_array" name="Local Relationship" />,
        'Array',
        <RelationshipSourceColumnCell
          tableName={tableDefinition?.table?.name}
          relationship={i}
          sourceType="local_array"
        />,
        <FaArrowRight className="fill-current text-sm text-muted" />,
        <RelationshipDestinationCell
          relationship={i}
          sourceType="local_array"
        />,
        <ModifyActions onEdit={onEdit} onDelete={onDelete} relationship={i} />,
      ]),
    ];
  }
  if (tableDefinition?.remote_relationships) {
    rowData = [
      ...rowData,
      ...tableDefinition?.remote_relationships?.map(i => {
        const [name, sourceType, type] = getRemoteRelationType(i);
        return [
          <NameColumnCell relationship={i} onClick={onClick} />,
          <SourceColumnCell {...{ type: sourceType, name }} />,
          type,
          <RelationshipSourceColumnCell
            tableName={tableDefinition?.table?.name}
            relationship={i}
            sourceType={sourceType}
          />,
          <FaArrowRight className="fill-current text-sm text-muted" />,
          <RelationshipDestinationCell
            relationship={i}
            sourceType={sourceType}
          />,
          <ModifyActions
            onEdit={onEdit}
            onDelete={onDelete}
            relationship={i}
          />,
        ];
      }),
    ];
  }

  return <CardedTable columns={columns} data={rowData} showActionCell />;
};

export default RelationshipsTable;
