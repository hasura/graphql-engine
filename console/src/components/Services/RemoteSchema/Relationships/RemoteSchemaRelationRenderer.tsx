import React, { useState } from 'react';
import { useGetAllRemoteSchemaRelationships } from '@/features/MetadataAPI';
import { RemoteSchemaRelationshipTable } from '@/features/RelationshipsTable';
import { Button } from '@/new-components/Button';
import { RiAddCircleFill } from 'react-icons/ri';
// eslint-disable-next-line no-restricted-imports
import { RemoteSchemaToDbForm } from '@/features/RemoteRelationships/RemoteSchemaRelationships/components/RemoteSchemaToDB';

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

  return (
    <>
      {isError ? (
        <div>Error in fetching remote schema relationships.</div>
      ) : isLoading ? (
        <div>Fetching remote schema relationships...</div>
      ) : (
        <RemoteSchemaRelationshipTable
          remoteSchemaRels={remoteSchemaRels ?? []}
          showActionCell={false}
          remoteSchema={remoteSchemaName}
        />
      )}

      {isFormOpen ? (
        <RemoteSchemaToDbForm
          sourceRemoteSchema={remoteSchemaName}
          closeHandler={() => setIsFormOpen(!isFormOpen)}
          onSuccess={() => setIsFormOpen(false)}
        />
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
