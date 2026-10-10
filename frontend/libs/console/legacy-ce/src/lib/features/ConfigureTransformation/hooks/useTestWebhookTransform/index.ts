import { useAuthFetchJson } from '@hasura/shared/hooks';
import {
  getValidateTransformOptions,
  ValidateTransformOptionsArgsType,
} from './utils';
import { useAppContext } from '@hasura/shared/context';

export const useTestWebhookTransform = () => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return (args: ValidateTransformOptionsArgsType) => {
    return fetchJson<Record<string, any>>(
      endpoints.metadata,
      getValidateTransformOptions(args),
    );
  };
};
