import {
  availableFeatureFlagIds,
  FeatureFlagFloatingButton,
  useIsFeatureFlagEnabled,
} from '@/features/FeatureFlags';
import { useGetAllRemoteSchemaRelationships } from '@/features/MetadataAPI';
import { RemoteSchemaRelationshipTable } from '@/features/RelationshipsTable';
import {
  RemoteRelOption,
  RemoteSchemaToDbForm,
  RemoteSchemaToRemoteSchemaForm,
} from '@/features/RemoteRelationships';
import { Button } from '@/new-components/Button';
import { IndicatorCard } from '@/new-components/IndicatorCard';
import React, { useState } from 'react';
import { RiAddCircleFill } from 'react-icons/ri';

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
  const [formState, setFormState] = useState<RemoteRelOption>('remoteSchema');
  const {
    enabled: ffEnabled,
    isLoading: ffIsLoading,
  } = useIsFeatureFlagEnabled(
    availableFeatureFlagIds.remoteSchemaRelationshipsId
  );

  if (isLoading || ffIsLoading) {
    return <div>Loading...</div>;
  }

  if (ffEnabled === undefined)
    return <div>Remote schema relationships are not yet enabled.</div>;

  if (ffEnabled === false)
    return (
      <div>
        Remote schema relationships are not enabled. To enable them, visit the
        Feature Flag section in Settings.
      </div>
    );

  if (isError) {
    return <div>Error in fetching remote schema relationships.</div>;
  }

  return (
    <>
      <FeatureFlagFloatingButton />

      {remoteSchemaRels?.length ? (
        <RemoteSchemaRelationshipTable
          remoteSchemaRels={remoteSchemaRels}
          showActionCell={false}
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
            closeHandler={() => setIsFormOpen(!isFormOpen)}
            relModeHandler={setFormState}
          />
        ) : (
          <RemoteSchemaToDbForm
            sourceRemoteSchema={remoteSchemaName}
            closeHandler={() => setIsFormOpen(!isFormOpen)}
            onSuccess={() => setIsFormOpen(false)}
            relModeHandler={setFormState}
          />
        )
      ) : (
        <Button
          icon={<RiAddCircleFill />}
          onClick={() => {
            setIsFormOpen(!isFormOpen);
          }}
          data-test="add-a-new-rs-relationship"
        >
          Add a new relationship
        </Button>
      )}
    </>
  );
};
