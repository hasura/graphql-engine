import { CardedTable } from '@/new-components/CardedTable';
import React, { ReactNode } from 'react';
import { FaArrowRight } from 'react-icons/fa';
import { rsToDbRelDef, rsToRsRelDef } from '@/metadata/types';
import ModifyActions from './components/ModifyActions';
import NameColumnCell from './components/NameColumnCell';
import RelationshipDestinationCell from './components/RelationshipDestinationCell';
import SourceColumnCell from './components/SourceColumnCell';
import { RelationshipType } from './types';
import { getRemoteSchemaRelationType } from './utils';
import FromRsCell from './components/FromRsCell';

export const columns = ['NAME', 'TARGET', 'TYPE', 'RELATIONSHIP', null];
export interface RelationshipsTableProps {
  remoteSchemaRels: ({ rsName: string } & (rsToDbRelDef | rsToRsRelDef))[];
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
  const rowData: ReactNode[][] = [];

  if (remoteSchemaRels) {
    const remoteRelationsOnTheSelectedRS = remoteSchemaRels.filter(
      x => x.rsName === remoteSchema
    );
    if (remoteRelationsOnTheSelectedRS.length)
      remoteRelationsOnTheSelectedRS.forEach(remoteRel => {
        const { type_name } = remoteRel;
        remoteRel.relationships.forEach(i => {
          const [name, sourceType, type] = getRemoteSchemaRelationType(i);
          const leafs =
            'to_source' in i.definition
              ? Object.keys(i.definition.to_source.field_mapping)
              : i.definition.to_remote_schema.lhs_fields;
          const value = [
            <NameColumnCell relationship={i} onClick={onClick} />,
            <SourceColumnCell {...{ type: sourceType, name }} />,
            type,
            <FromRsCell rsName={type_name} leafs={leafs} />,
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

          rowData.push(value);
        });
      });
  }

  if (rowData?.length)
    return (
      <CardedTable
        columns={columns}
        data={rowData}
        showActionCell={showActionCell}
        data-test="remote-schema-relationships-table"
      />
    );
  return <div>No remote schema relationships found!</div>;
};

export default RemoteSchemaRelationshipTable;
