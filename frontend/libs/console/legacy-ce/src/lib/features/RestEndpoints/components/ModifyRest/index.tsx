import React from 'react';
import { Navigate, useParams } from 'react-router';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { RestEndpointForm } from '../Form';
import { REST_API_LIST_PATH, RestEndpointFormState } from '../../types';
import { useEditRestEndpoint, useMetadata } from '@hasura/metadata/api';
import { allowedQueriesCollection, RestEndpoint } from '@hasura/shared/types';
import { Skeleton } from '@radix-ui/themes';

const ModifyRest: React.FC = () => {
  const params = useParams();
  const { data: meta, isFetching } = useMetadata();
  const { editRestEndpoint, isPending } = useEditRestEndpoint();

  const editEndpoint = (
    newEntry: RestEndpoint,
    request: string,
    cb: () => void,
    oldEntry: RestEndpoint,
  ) => editRestEndpoint({ oldEntry, newEntry, request }, { onSuccess: cb });

  const currentPageName = params.name;
  const currentEndpoints = meta?.metadata.rest_endpoints ?? [];
  const currentCollections = meta?.metadata.query_collections;
  const currentRestEndpointEntry =
    currentEndpoints.find((et) => et.name === currentPageName) ?? null;
  const currentEndpointQuery = currentCollections
    ?.find((qce) => qce.name === allowedQueriesCollection)
    ?.definition?.queries?.find((qc) => qc.name === currentPageName);

  if (!currentRestEndpointEntry || !editEndpoint) {
    return <Navigate to={REST_API_LIST_PATH} />;
  }

  const formState: RestEndpointFormState = {
    name: currentRestEndpointEntry.name,
    comment: currentRestEndpointEntry.comment,
    url: currentRestEndpointEntry.url,
    methods: currentRestEndpointEntry.methods,
    request: currentEndpointQuery?.query,
  };

  const formSubmitHandler = (restEndpoint, request, cb) =>
    editEndpoint(restEndpoint, request, cb, currentRestEndpointEntry);

  return (
    <Analytics name={'FormRestEdit'} {...REDACT_EVERYTHING}>
      <Skeleton loading={isFetching}>
        <RestEndpointForm
          mode="edit"
          formState={formState}
          loading={isPending}
          onSubmit={formSubmitHandler}
        />
      </Skeleton>
    </Analytics>
  );
};

export default ModifyRest;
