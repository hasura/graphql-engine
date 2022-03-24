import React from 'react';
import { useGetAllRemoteSchemaRelationships } from '@/features/MetadataAPI';
import { RemoteSchemaRelationshipTable } from '@/features/RelationshipsTable';

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
  if (isError) {
    return <div>Error in fetching remote schema relationships.</div>;
  }
  if (isLoading) {
    return <div>Loading...</div>;
  }
  if (!remoteSchemaRels || !remoteSchemaRels.length) {
    return <div>No remote schema relationships found!</div>;
  }
  return (
    <RemoteSchemaRelationshipTable
      remoteSchemaRels={remoteSchemaRels}
      showActionCell={false}
      remoteSchema={remoteSchemaName}
    />
  );
};
