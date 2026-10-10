import { useState } from 'react';
import { RiAddCircleFill } from 'react-icons/ri';
import {
  RemoteSchemaRelationshipTable,
  ExistingRelationshipMeta,
} from '../../../RelationshipsTable';
import {
  Button,
  IndicatorCard,
  SkeletonList,
  useDestructiveConfirm,
} from '@hasura/shared/ui';
import {
  RemoteRelOption,
  RemoteSchemaToDbForm,
  RemoteSchemaToRemoteSchemaForm,
} from '../../../RemoteRelationships';
import { InconsistentBadge } from '../InconsistentBadge';
import {
  useDeleteRemoteSchemaRemoteRelationship,
  useInconsistentMetadata,
} from '@hasura/metadata/api';
import {
  findInconsistentRemoteSchema,
  MetadataSelectors,
} from '@hasura/metadata/helpers';
import { useMetadata } from '@hasura/metadata/api';
import { RemoteRelationship } from '@hasura/shared/types';

type RemoteSchemaRelationRendererProp = {
  remoteSchemaName: string;
};

export const RemoteSchemaRelationRenderer = ({
  remoteSchemaName,
}: RemoteSchemaRelationRendererProp) => {
  const {
    data: remoteSchema,
    isLoading,
    isError,
  } = useMetadata(MetadataSelectors.findRemoteSchema(remoteSchemaName));

  const [isFormOpen, setIsFormOpen] = useState(false);

  const [existingRelationship, setExistingRelationship] = useState<{
    relationship?: RemoteRelationship;
    rsType?: string;
  }>({});
  const [formState, setFormState] = useState<RemoteRelOption>('remoteSchema');
  const { data: inconsistentMetadata } = useInconsistentMetadata();

  const mutation = useDeleteRemoteSchemaRemoteRelationship();
  const destructiveConfirm = useDestructiveConfirm();

  if (isLoading) {
    return <SkeletonList count={5} />;
  }

  const openForm = ({
    relationship,
    relationshipType,
    rsType,
  }: Partial<ExistingRelationshipMeta>) => {
    setFormState((relationshipType ?? 'remoteSchema') as RemoteRelOption);
    setExistingRelationship({
      relationship,
      rsType,
    });
    setIsFormOpen(true);
  };

  const onDelete = ({
    relationship,
    rsType,
  }: Omit<ExistingRelationshipMeta, 'relationshipType'>) => {
    destructiveConfirm({
      resourceName: relationship.name,
      resourceType: 'relationship',
      onConfirm: () => {
        return mutation
          .mutateAsync({
            remote_schema: remoteSchemaName,
            type_name: rsType,
            name: relationship.name,
          })
          .then(() => {
            setIsFormOpen(false);
            return true;
          })
          .catch(() => false);
      },
    });
  };

  if (isError) {
    return (
      <IndicatorCard status="negative" showIcon>
        Error in fetching remote schema relationships.
      </IndicatorCard>
    );
  }

  const inconsistencyDetails = findInconsistentRemoteSchema(
    inconsistentMetadata?.inconsistent_objects,
    remoteSchemaName,
  );

  return (
    <>
      {inconsistencyDetails && (
        <InconsistentBadge inconsistencyDetails={inconsistencyDetails} />
      )}

      {isFormOpen ? null : remoteSchema?.remote_relationships?.length ? (
        <RemoteSchemaRelationshipTable
          showActionCell
          onEdit={(props) => {
            openForm(props);
          }}
          onDelete={onDelete}
          remoteSchema={remoteSchema}
        />
      ) : (
        <div className="w-full sm:w-9/12">
          <IndicatorCard status="info">
            No remote schema relationships found!
          </IndicatorCard>
          <br />
        </div>
      )}
      {isFormOpen ? (
        formState === 'remoteSchema' ? (
          <RemoteSchemaToRemoteSchemaForm
            sourceRemoteSchema={remoteSchemaName}
            existingRelationship={existingRelationship.relationship}
            typeName={existingRelationship.rsType}
            closeHandler={() => setIsFormOpen(!isFormOpen)}
            onSuccess={() => setIsFormOpen(false)}
            relModeHandler={setFormState}
          />
        ) : (
          <RemoteSchemaToDbForm
            sourceRemoteSchema={remoteSchemaName}
            existingRelationship={existingRelationship.relationship}
            typeName={existingRelationship.rsType}
            closeHandler={() => setIsFormOpen(!isFormOpen)}
            onSuccess={() => setIsFormOpen(false)}
            relModeHandler={setFormState}
          />
        )
      ) : (
        !inconsistencyDetails && (
          <Button
            mode="default"
            leftIcon={RiAddCircleFill}
            onClick={() => {
              openForm({
                rsType: 'remoteSchema',
              });
            }}
            data-test="add-a-new-rs-relationship"
          >
            Add a new relationship
          </Button>
        )
      )}
    </>
  );
};
