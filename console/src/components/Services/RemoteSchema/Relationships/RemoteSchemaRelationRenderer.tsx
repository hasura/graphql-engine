import React, { useState } from 'react';
import { RiAddCircleFill } from 'react-icons/ri';
import { FeatureFlagFloatingButton } from '@/features/FeatureFlags';
import {
  allowedMetadataTypes,
  useGetAllRemoteSchemaRelationships,
  useMetadataMigration,
} from '@/features/MetadataAPI';
import {
  RemoteSchemaRelationshipTable,
  ExistingRelationshipMeta,
} from '@/features/RelationshipsTable';
import { Button } from '@/new-components/Button';
import {
  RemoteRelOption,
  RemoteSchemaToDbForm,
  RemoteSchemaToRemoteSchemaForm,
} from '@/features/RemoteRelationships';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import { useFireNotification } from '@/new-components/Notifications';
import { getConfirmation } from '@/components/Common/utils/jsUtils';

type RemoteSchemaRelationRendererProp = {
  remoteSchemaName: string;
};

export const RemoteSchemaRelationRenderer = ({
  remoteSchemaName,
}: RemoteSchemaRelationRendererProp) => {
  const {
    data: remoteSchemaRels,
    isLoading,
    isError,
  } = useGetAllRemoteSchemaRelationships();

  const [isFormOpen, setIsFormOpen] = useState(false);

  const [
    existingRelationship,
    setExistingRelationship,
  ] = useState<ExistingRelationshipMeta>({
    relationshipName: '',
    rsType: '',
  });
  const [formState, setFormState] = useState<RemoteRelOption>('remoteSchema');

  const { fireNotification } = useFireNotification();
  const mutation = useMetadataMigration({
    onSuccess: () => {
      fireNotification({
        title: 'Success!',
        message: 'Relationship deleted successfully',
        type: 'success',
      });
    },
    onError: (error: Error) => {
      fireNotification({
        title: 'Error',
        message: error?.message ?? 'Error while deleting the relationship',
        type: 'error',
      });
    },
  });

  if (isLoading) {
    return <div>Loading...</div>;
  }

  const openForm = ({
    relationshipName,
    rsType,
    relationshipType,
  }: ExistingRelationshipMeta) => {
    setFormState((relationshipType ?? 'remoteSchema') as RemoteRelOption);
    setExistingRelationship({ relationshipName, rsType });
    setIsFormOpen(true);
  };

  const onDelete = ({ relationshipName, rsType }: ExistingRelationshipMeta) => {
    const confirmMessage = `This will permanently delete the ${relationshipName} from Hasura`;
    const isOk = getConfirmation(confirmMessage, true, relationshipName);
    if (!isOk) {
      return;
    }

    mutation.mutate({
      query: {
        type: 'delete_remote_schema_remote_relationship' as allowedMetadataTypes,
        args: {
          remote_schema: remoteSchemaName,
          type_name: rsType,
          name: relationshipName,
        },
      },
    });
    setIsFormOpen(false);
  };

  if (isError) {
    return <div>Error in fetching remote schema relationships.</div>;
  }

  return (
    <>
      <FeatureFlagFloatingButton />

      {remoteSchemaRels?.length ? (
        <RemoteSchemaRelationshipTable
          remoteSchemaRels={remoteSchemaRels}
          showActionCell
          onEdit={props => {
            openForm(props);
          }}
          onDelete={onDelete}
          remoteSchema={remoteSchemaName}
        />
      ) : (
        <>
          <IndicatorCard status="info">
            No remote schema relationships found!
          </IndicatorCard>
          <br />
        </>
      )}
      {isFormOpen ? (
        formState === 'remoteSchema' ? (
          <RemoteSchemaToRemoteSchemaForm
            sourceRemoteSchema={remoteSchemaName}
            existingRelationshipName={existingRelationship.relationshipName}
            typeName={existingRelationship.rsType}
            closeHandler={() => setIsFormOpen(!isFormOpen)}
            onSuccess={() => setIsFormOpen(false)}
            relModeHandler={setFormState}
          />
        ) : (
          <RemoteSchemaToDbForm
            sourceRemoteSchema={remoteSchemaName}
            existingRelationshipName={existingRelationship.relationshipName}
            typeName={existingRelationship.rsType}
            closeHandler={() => setIsFormOpen(!isFormOpen)}
            onSuccess={() => setIsFormOpen(false)}
            relModeHandler={setFormState}
          />
        )
      ) : (
        <Button
          icon={<RiAddCircleFill />}
          onClick={() => {
            openForm({});
          }}
          data-test="add-a-new-rs-relationship"
        >
          Add a new relationship
        </Button>
      )}
    </>
  );
};
