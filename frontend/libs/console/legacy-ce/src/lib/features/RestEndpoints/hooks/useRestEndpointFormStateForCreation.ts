import { parse, print } from 'graphql';
import type {
  RestEndpointCreateAction,
  RestEndpointFormState,
  RestEndpointFormStateHook,
} from '../types';
import { useSearchParams } from 'react-router';
import { getLSItem } from '@hasura/shared/utils';
import { LS_KEYS } from '@hasura/shared/types';

const useRestEndpointFormStateForCreation = (
  createEndpoint: RestEndpointCreateAction,
): RestEndpointFormStateHook => {
  const [searchParams] = useSearchParams();

  const formState: RestEndpointFormState = {};

  try {
    if (searchParams.get('from') === 'graphiql') {
      const rawQuery = getLSItem(LS_KEYS.graphiqlQuery);
      formState.request = rawQuery ? print(parse(rawQuery)) : undefined;
    }
  } catch (e) {
    // ignore
  }

  return { formState, formSubmitHandler: createEndpoint };
};

export default useRestEndpointFormStateForCreation;
