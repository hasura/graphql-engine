import React, { useState } from 'react';
import { useGetAllRemoteSchemaRelationships } from '@/features/MetadataAPI';
import { RemoteSchemaRelationshipTable } from '@/features/RelationshipsTable';
import { Button } from '@/new-components/Button';
import { RiAddCircleFill } from 'react-icons/ri';
import {
  RemoteSchemaToRemoteSchemaForm,
  RemoteRelOption,
  RemoteSchemaToDbForm,
} from '@/features/RemoteRelationships';
import { IndicatorCard } from '@/new-components/IndicatorCard';

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

  if (isError) {
    return <div>Error in fetching remote schema relationships.</div>;
  }
  if (isLoading) {
    return <div>Loading...</div>;
  }

  return (
    <>
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
