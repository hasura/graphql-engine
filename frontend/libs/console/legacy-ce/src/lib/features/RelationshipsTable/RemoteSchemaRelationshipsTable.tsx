import { CardedTable } from '../../new-components/CardedTable';
import React, { ReactNode } from 'react';
import { IconType } from 'react-icons';
import {
  FaArrowRight,
  FaColumns,
  FaDatabase,
  FaFont,
  FaPlug,
  FaTable,
} from 'react-icons/fa';
import { FiType } from 'react-icons/fi';
import { rsToDbRelDef, rsToRsRelDef } from '../../metadata/types';
import { IndicatorCard } from '../../new-components/IndicatorCard';
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
  onEdit?: ({ relationshipName, rsType }: ExistingRelationshipMeta) => void;
  onDelete?: ({ relationshipName, rsType }: ExistingRelationshipMeta) => void;
  onClick?: (relationship: RelationshipType) => void;
  showActionCell?: boolean;
}

export interface ExistingRelationshipMeta {
  relationshipName?: string;
  rsType?: string;
  relationshipType?: string;
}

export const RemoteSchemaRelationshipTable = ({
  remoteSchemaRels,
  remoteSchema,
  onEdit = () => {},
  onDelete = () => {},
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

        remoteRel.relationships.forEach(relationship => {
          const [name, sourceType, type] =
            getRemoteSchemaRelationType(relationship);
          const relType =
            'to_source' in relationship.definition
              ? 'to_source'
              : 'to_remote_schema';
          const leafs =
            'to_source' in relationship.definition
              ? Object.keys(relationship.definition.to_source.field_mapping)
              : relationship.definition.to_remote_schema.lhs_fields;
          const value = [
            <NameColumnCell
              relationship={relationship}
              onClick={() => {
                onEdit({
                  relationshipName: relationship.name,
                  rsType: type_name,
                  relationshipType:
                    relType === 'to_source' ? 'remoteDB' : 'remoteSchema',
                });
              }}
            />,
            <SourceColumnCell {...{ type: sourceType, name }} />,
            type,
            <FromRsCell rsName={type_name} leafs={leafs} />,
            <FaArrowRight className="fill-current text-sm text-muted" />,

            <RelationshipDestinationCell
              relationship={relationship}
              sourceType={sourceType}
            />,
          ];
          if (showActionCell) {
            value.push(
              <ModifyActions
                onEdit={() =>
                  onEdit({
                    relationshipName: relationship.name,
                    rsType: type_name,
                    relationshipType:
                      relType === 'to_source' ? 'remoteDB' : 'remoteSchema',
                  })
                }
                onDelete={() =>
                  onDelete({
                    relationshipName: relationship.name,
                    rsType: type_name,
                  })
                }
                relationship={relationship}
              />
            );
          }

          rowData.push(value);
        });
      });
  }

  const legend: { Icon: IconType; name: string }[] = [
    {
      Icon: FaPlug,
      name: 'Remote Schema',
    },
    {
      Icon: FiType,
      name: 'Type',
    },
    {
      Icon: FaFont,
      name: 'Field',
    },
    {
      Icon: FaDatabase,
      name: 'Database',
    },
    {
      Icon: FaTable,
      name: 'Table',
    },
    {
      Icon: FaColumns,
      name: 'Column',
    },
  ];

  if (rowData?.length)
    return (
      <div>
        <CardedTable
          columns={columns}
          data={rowData}
          showActionCell={showActionCell}
          data-test="remote-schema-relationships-table"
        />
        <div className="text-right mb-4">
          {legend.map(item => {
            const { Icon, name } = item;
            return (
              <span>
                <Icon
                  className="mr-1 ml-4 text-sm"
                  style={{ strokeWidth: 4.5 }}
                />
                {name}
              </span>
            );
          })}
        </div>
      </div>
    );
  return (
    <>
      <IndicatorCard status="info">
        No remote schema relationships found!
      </IndicatorCard>
      <br />
    </>
  );
};

export default RemoteSchemaRelationshipTable;
