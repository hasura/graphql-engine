import React from 'react';
import { useSearchParams } from 'react-router';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { RestEndpointForm } from '../Form';
import { RestEndpointFormState } from '../../types';
import { useAddRestEndpoint } from '@hasura/metadata/api';
import { LS_KEYS, RestEndpoint } from '@hasura/shared/types';
import { getLSItem } from '@hasura/shared/utils';
import { parse, print } from 'graphql';

const CreateRestView: React.FC = () => {
  const [searchParams] = useSearchParams();
  const { addRestEndpoint, isPending } = useAddRestEndpoint();

  const formState: RestEndpointFormState = {};

  try {
    if (searchParams.get('from') === 'graphiql') {
      const rawQuery = getLSItem(LS_KEYS.graphiqlQuery);
      formState.request = rawQuery ? print(parse(rawQuery)) : undefined;
    }
  } catch (e) {
    // ignore
  }

  const createEndpoint = (
    restEndpoint: RestEndpoint,
    request: string,
    cb: () => void,
  ) => addRestEndpoint({ entry: restEndpoint, request }, { onSuccess: cb });

  return (
    <Analytics name={'FormRestCreate'} {...REDACT_EVERYTHING}>
      <RestEndpointForm
        mode="create"
        formState={formState}
        loading={isPending}
        onSubmit={createEndpoint}
      />
    </Analytics>
  );
};

export default CreateRestView;
