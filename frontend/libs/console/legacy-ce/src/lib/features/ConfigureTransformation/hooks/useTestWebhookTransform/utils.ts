import { isJsonString } from '@hasura/shared/utils';
import {
  ActionRequestTransform,
  RequestTransformMethod,
} from '@hasura/shared/types';
import { NameValue } from '@hasura/shared/types';
import { RequestTransformStateBody } from '../../stateDefaults';
import { getPairsObjFromArray } from '../../utils';

const getTransformBodyServer = (reqBody: RequestTransformStateBody) => {
  if (reqBody.action === 'remove') return { action: reqBody.action };

  if (reqBody.action === 'transform')
    return { action: reqBody.action, template: reqBody.template };

  return {
    action: reqBody.action,
    form_template: getPairsObjFromArray(reqBody.form_template ?? []),
  };
};

const getTransformer = ({
  version,
  transformerBody,
  transformerUrl,
  requestMethod,
  queryParams,
}: {
  version: '1' | '2';
  transformerBody?: RequestTransformStateBody;
  transformerUrl: string;
  requestMethod: RequestTransformMethod | null | undefined;
  queryParams?: NameValue[] | string;
}): ActionRequestTransform => {
  const query_params = Array.isArray(queryParams)
    ? getPairsObjFromArray(queryParams.filter((v) => v.name))
    : queryParams;
  const method = requestMethod || 'GET';

  return version === '1'
    ? {
        version: 1,
        query_params,
        body: transformerBody?.template || undefined,
        url: transformerUrl,
        method,
        template_engine: 'Kriti',
      }
    : {
        version: 2,
        body: transformerBody
          ? getTransformBodyServer(transformerBody)
          : undefined,
        url: transformerUrl || undefined,
        method,
        query_params,
        template_engine: 'Kriti',
      };
};

const generateValidateTransformQuery = (
  transformer: ActionRequestTransform,
  requestPayload: Record<string, any> | null = null,
  webhookUrl: string,
  sessionVars?: NameValue[],
  isEnvVar?: boolean,
  envVars?: NameValue[],
) => {
  return {
    type: 'test_webhook_transform',
    args: {
      webhook_url: isEnvVar ? { from_env: webhookUrl } : webhookUrl,
      body: requestPayload,
      env: envVars
        ? getPairsObjFromArray(envVars.filter((item) => item.name))
        : undefined,
      session_variables: sessionVars
        ? getPairsObjFromArray(sessionVars.filter((sv) => sv.name))
        : undefined,
      request_transform: transformer,
    },
  };
};

export type ValidateTransformOptionsArgsType = {
  version: '1' | '2';
  inputPayloadString: string;
  webhookUrl: string;
  envVarsFromContext?: NameValue[];
  sessionVarsFromContext?: NameValue[];
  transformerBody?: RequestTransformStateBody;
  requestUrl?: string;
  queryParams?: NameValue[] | string;
  isEnvVar?: boolean;
  requestMethod?: RequestTransformMethod | null;
};

export const getValidateTransformOptions = ({
  version,
  inputPayloadString,
  webhookUrl,
  envVarsFromContext,
  sessionVarsFromContext,
  transformerBody,
  requestUrl,
  queryParams,
  isEnvVar,
  requestMethod,
}: ValidateTransformOptionsArgsType) => {
  const requestPayload = isJsonString(inputPayloadString)
    ? JSON.parse(inputPayloadString)
    : null;
  const transformerUrl = requestUrl
    ? `{{$base_url}}${requestUrl}`
    : `{{$base_url}}`;

  const finalReqBody = generateValidateTransformQuery(
    getTransformer({
      version,
      transformerBody,
      transformerUrl,
      requestMethod,
      queryParams,
    }),
    requestPayload,
    webhookUrl,
    sessionVarsFromContext,
    isEnvVar,
    envVarsFromContext,
  );

  const options: RequestInit = {
    method: 'POST',
    body: JSON.stringify(finalReqBody),
  };

  return options;
};
