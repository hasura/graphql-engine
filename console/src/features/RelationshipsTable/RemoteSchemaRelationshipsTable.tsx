import { CardedTable } from '@/new-components/CardedTable';
import React, { ReactNode } from 'react';
import { FaArrowRight } from 'react-icons/fa';
import { RemoteRelationship } from '@/metadata/types';
import ModifyActions from './components/ModifyActions';
import NameColumnCell from './components/NameColumnCell';
import RelationshipDestinationCell from './components/RelationshipDestinationCell';
import SourceColumnCell from './components/SourceColumnCell';
import RelationshipSourceColumnCell from './components/RelationshipSourceCell';
import { RelationshipType } from './types';
import { getRemoteRelationType } from './utils';

export const columns = ['NAME', 'SOURCE', 'TYPE', 'RELATIONSHIP', null];

type TRemoteSchemaRel = { table_name: string } & RemoteRelationship;
export interface RelationshipsTableProps {
  remoteSchemaRels: TRemoteSchemaRel[];
  remoteSchema: string;
  onEdit?: (relationship: RelationshipType) => void;
  onDelete?: (relationship: RelationshipType) => void;
  onClick?: (relationship: RelationshipType) => void;
  showActionCell?: boolean;
}

export const RemoteSchemaRelationshipTable = ({
  remoteSchemaRels,
  remoteSchema,
  onEdit = () => {},
  onDelete = () => {},
  onClick = () => {},
  showActionCell = true,
}: RelationshipsTableProps) => {
  let rowData: ReactNode[][] = [];
  rowData = [
    ...rowData,
    ...remoteSchemaRels
      .filter(x => x?.definition?.remote_schema === remoteSchema)
      .map(i => {
        const [name, sourceType, type] = getRemoteRelationType(i);
        const value = [
          <NameColumnCell relationship={i} onClick={onClick} />,
          <SourceColumnCell {...{ type: sourceType, name }} />,
          type,
          <RelationshipSourceColumnCell
            tableName={i?.table_name}
            relationship={i}
            sourceType={sourceType}
          />,
          <FaArrowRight className="fill-current text-sm text-muted" />,
          <RelationshipDestinationCell
            relationship={i}
            sourceType={sourceType}
          />,
        ];
        if (showActionCell) {
          value.push(
            <ModifyActions
              onEdit={onEdit}
              onDelete={onDelete}
              relationship={i}
            />
          );
        }
        return value;
      }),
  ];
  return (
    <CardedTable
      columns={columns}
      data={rowData}
      showActionCell={showActionCell}
    />
  );
};

export default RemoteSchemaRelationshipTable;
