import { CardedTable, IndicatorCard } from '@hasura/shared/ui';
import { ReactNode } from 'react';
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
import ModifyActions from './components/ModifyActions';
import NameColumnCell from './components/NameColumnCell';
import RelationshipDestinationCell from './components/RelationshipDestinationCell';
import SourceColumnCell from './components/SourceColumnCell';
import { RelationshipType } from './types';
import { getRemoteSchemaRelationType } from './utils';
import FromRsCell from './components/FromRsCell';
import { RemoteRelationship, RemoteSchema } from '@hasura/shared/types';
import { Flex } from '@radix-ui/themes';

export const columns = ['NAME', 'TARGET', 'TYPE', 'RELATIONSHIP', null];

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

export interface RelationshipsTableProps {
  remoteSchema: RemoteSchema;
  onEdit: (props: ExistingRelationshipMeta) => void;
  onDelete: (props: Omit<ExistingRelationshipMeta, 'relationshipType'>) => void;
  onClick?: (relationship: RelationshipType) => void;
  showActionCell?: boolean;
}

export interface ExistingRelationshipMeta {
  relationship: RemoteRelationship;
  rsType: string;
  relationshipType: 'remoteDB' | 'remoteSchema';
}

export const RemoteSchemaRelationshipTable = ({
  remoteSchema,
  onEdit,
  onDelete,
  showActionCell = true,
}: RelationshipsTableProps) => {
  const rowData: ReactNode[][] = [];

  if (remoteSchema.remote_relationships?.length) {
    remoteSchema.remote_relationships.forEach((remoteRelationship) => {
      remoteRelationship.relationships.forEach((relationship) => {
        const [name, sourceType, type] =
          getRemoteSchemaRelationType(relationship);
        const relType =
          'to_source' in relationship.definition
            ? 'to_source'
            : 'to_remote_schema';
        const leafs =
          'to_source' in relationship.definition
            ? Object.keys(relationship.definition.to_source?.field_mapping)
            : 'to_remote_schema' in relationship.definition
              ? relationship.definition.to_remote_schema.lhs_fields
              : relationship.definition.hasura_fields;
        const onEditCell = () =>
          onEdit({
            relationship,
            rsType: remoteRelationship.type_name,
            relationshipType:
              relType === 'to_source' ? 'remoteDB' : 'remoteSchema',
          });
        const value = [
          <NameColumnCell
            key={`name-${relationship.name}`}
            relationship={relationship}
            onClick={onEditCell}
          />,
          <SourceColumnCell
            key={`source-${relationship.name}`}
            type={sourceType}
            name={name}
          />,
          type,
          <FromRsCell
            key={`from-${relationship.name}`}
            leafs={leafs}
            rsType={remoteRelationship.type_name}
          />,
          <FaArrowRight
            key={`arrow-${relationship.name}`}
            className="fill-current text-sm text-muted"
          />,

          <RelationshipDestinationCell
            key={`dest-${relationship.name}`}
            relationship={relationship}
          />,
        ];

        if (showActionCell) {
          value.push(
            <ModifyActions
              key={`actions-${relationship.name}`}
              onEdit={onEditCell}
              onDelete={() =>
                onDelete({
                  relationship,
                  rsType: remoteRelationship.type_name,
                })
              }
              relationship={relationship}
            />,
          );
        }

        rowData.push(value);
      });
    });
  }

  if (rowData?.length)
    return (
      <div>
        <CardedTable
          columns={columns}
          data={rowData}
          data-test="remote-schema-relationships-table"
        />
        <Flex align="center" justify="end" className="my-4">
          {legend.map((item) => {
            const { Icon, name } = item;
            return (
              <Flex key={name} align="center" gap="2">
                <Icon
                  className="mr-1 ml-4 text-sm"
                  style={{ strokeWidth: 4.5 }}
                />
                {name}
              </Flex>
            );
          })}
        </Flex>
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
