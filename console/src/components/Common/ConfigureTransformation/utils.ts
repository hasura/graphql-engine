import { RequestTransform, RequestTransformMethod } from '@/metadata/types';
import { getLSItem, setLSItem, LS_KEYS } from '@/utils/localStorage';
import {
  defaultRequestContentType,
  GraphiQlHeader,
  KeyValuePair,
  RequestTransformState,
} from './stateDefaults';
import { isEmpty, isJsonString } from '../utils/jsUtils';
import { Nullable } from '../utils/tsUtils';

export const getPairsObjFromArray = (pairs: KeyValuePair[]) => {
  let obj = {};

  pairs.forEach(({ name, value }) => {
    if (!!name && !!value) {
      const pair = { [name]: value };
      obj = { ...obj, ...pair };
    }
  });

  return obj;
};

export const addPlaceholderValue = (pairs: KeyValuePair[]) => {
  if (pairs.length) {
    const lastVal = pairs[pairs.length - 1];
    if (lastVal.name && lastVal.value) {
      pairs.push({ name: '', value: '' });
    }
  } else {
    pairs.push({ name: '', value: '' });
  }
  return pairs;
};

const getSessionVarsArrayFromGraphiQL = () => {
  const lsHeadersString =
    getLSItem(LS_KEYS.apiExplorerConsoleGraphQLHeaders) ?? '';
  const headers: GraphiQlHeader[] = isJsonString(lsHeadersString)
    ? JSON.parse(lsHeadersString)
    : [];
  let sessionVars: KeyValuePair[] = [];
  if (Array.isArray(headers)) {
    sessionVars = headers
      .filter(
        (header: GraphiQlHeader) =>
          header.isActive && header.key?.toLowerCase().startsWith('x-hasura')
      )
      .map((header: GraphiQlHeader) => ({
        name: header.key?.toLowerCase(),
        value: header.value,
      }));
  }
  return sessionVars;
};

const getEnvVarsArrayFromLS = () => {
  const lsEnvString = getLSItem(LS_KEYS.webhookTransformEnvVars) ?? '';
  const envVars: KeyValuePair[] = isJsonString(lsEnvString)
    ? JSON.parse(lsEnvString)
    : [];
  return envVars;
};

export const getSessionVarsFromLS = () =>
  isEmpty(getSessionVarsArrayFromGraphiQL())
    ? [{ name: '', value: '' }]
    : [...getSessionVarsArrayFromGraphiQL(), { name: '', value: '' }];

export const getEnvVarsFromLS = () =>
  isEmpty(getEnvVarsArrayFromLS())
    ? [{ name: '', value: '' }]
    : [...getEnvVarsArrayFromLS(), { name: '', value: '' }];

export const setEnvVarsToLS = (envVars: KeyValuePair[]) => {
  const validEnvVars = envVars.filter(
    e => !isEmpty(e.name) && !isEmpty(e.value)
  );
  setLSItem(`${LS_KEYS.webhookTransformEnvVars}`, JSON.stringify(validEnvVars));
};

export const getArrayFromServerPairObject = (
  pairs: Nullable<Record<string, string>>
): KeyValuePair[] => {
  const transformArray: KeyValuePair[] = [];
  if (pairs && Object.keys(pairs).length !== 0) {
    Object.entries(pairs).forEach(([key, value]) => {
      transformArray.push({ name: key, value });
    });
  }
  transformArray.push({ name: '', value: '' });
  return transformArray;
};

const checkEmptyString = (val?: string) => {
  return val && val !== '' ? val : undefined;
};

const getUrlWithBasePrefix = (val?: string) => {
  return val ? `{{$base_url}}${val}` : undefined;
};

export const getRequestTransformObject = (
  transformState: RequestTransformState
) => {
  const isRequestUrlTransform = transformState.isRequestUrlTransform;
  const isRequestPayloadTransform = transformState.isRequestPayloadTransform;
  const enableRequestBody = transformState.enableRequestBody;

  if (!isRequestUrlTransform && !isRequestPayloadTransform) return null;

  let obj: RequestTransform = {
    version: 2,
    template_engine: transformState.templatingEngine,
  };

  if (isRequestUrlTransform) {
    obj = {
      ...obj,
      method: transformState.requestMethod,
      url: getUrlWithBasePrefix(transformState.requestUrl),
      query_params: getPairsObjFromArray(transformState.requestQueryParams),
    };
    if (transformState.requestMethod === 'GET') {
      obj = {
        ...obj,
        request_headers: {
          remove_headers: ['content-type'],
        },
      };
    }
  }

  if (isRequestPayloadTransform) {
    obj = {
      ...obj,
      body: enableRequestBody
        ? {
            action: 'transform',
            template: checkEmptyString(transformState.requestBody),
          }
        : {
            action: 'remove',
          },
      content_type: transformState.requestContentType,
    };
  }

  return obj;
};

const getErrorFromCode = (data: Record<string, any>) => {
  const errorCode = data.code ? data.code : '';
  const errorMsg = data.error ? data.error : '';
  return `${errorCode}: ${errorMsg}`;
};

const getErrorFromBody = (data: Record<string, any>) => {
  const errorObj = data.body[0];
  const errorCode = errorObj?.error_code;
  const errorMsg = errorObj?.message;
  const stPos = errorObj?.source_position?.start_line
    ? `, starts line ${errorObj?.source_position?.start_line}, column ${errorObj?.source_position?.start_column}`
    : ``;
  const endPos = errorObj?.source_position?.end_line
    ? `, ends line ${errorObj?.source_position?.end_line}, column ${errorObj?.source_position?.end_column}`
    : ``;
  return `${errorCode}: ${errorMsg} ${stPos} ${endPos}`;
};

export const parseValidateApiData = (
  requestData: Record<string, any>,
  setError: (error: string) => void,
  setUrl?: (data: string) => void,
  setBody?: (data: string) => void
) => {
  if (requestData?.code) {
    const errorMessage = getErrorFromCode(requestData);
    setError(errorMessage);
  } else if (requestData?.body?.[0]?.error_code) {
    const errorMessage = getErrorFromBody(requestData);
    setError(errorMessage);
  } else if (requestData?.webhook_url || requestData?.body) {
    setError('');
    if (setUrl && requestData?.webhook_url) {
      setUrl(requestData?.webhook_url);
    }
    if (setBody && requestData?.body) {
      setBody(JSON.stringify(requestData?.body, null, 2));
    }
  } else {
    const errorMessage = `Error during validation: ${requestData}`;
    setError(errorMessage);
  }
};

// fields for `request_transform` key in `test_webhook_transform` api
type RequestTransformerFields = {
  url?: string;
  method?: Nullable<RequestTransformMethod>;
  query_params?: Record<string, string>;
  template_engine?: string;
};

type RequestTransformerV1 = RequestTransformerFields & {
  version: 1;
  body?: string;
};

type RequestTransformerV2 = RequestTransformerFields & {
  version: 2;
  body?: Record<string, string>;
};

type RequestTransformer = RequestTransformerV1 | RequestTransformerV2;

const getTransformer = (
  version: 1 | 2,
  transformerBody?: string,
  transformerUrl?: string,
  requestMethod?: Nullable<RequestTransformMethod>,
  queryParams?: KeyValuePair[]
): RequestTransformer => {
  return version === 1
    ? {
        version,
        body: checkEmptyString(transformerBody),
        url: checkEmptyString(transformerUrl),
        method: requestMethod,
        query_params: queryParams
          ? getPairsObjFromArray(queryParams)
          : undefined,
        template_engine: 'Kriti',
      }
    : {
        version,
        body: {
          action: 'transform',
          template: checkEmptyString(transformerBody) ?? '{}',
        },
        url: checkEmptyString(transformerUrl),
        method: requestMethod,
        query_params: queryParams
          ? getPairsObjFromArray(queryParams)
          : undefined,
        template_engine: 'Kriti',
      };
};

const generateValidateTransformQuery = (
  transformer: RequestTransformer,
  requestPayload: Nullable<Record<string, any>> = null,
  webhookUrl: string,
  sessionVars?: KeyValuePair[],
  isEnvVar?: boolean,
  envVars?: KeyValuePair[]
) => {
  return {
    type: 'test_webhook_transform',
    args: {
      webhook_url: isEnvVar ? { from_env: webhookUrl } : webhookUrl,
      body: requestPayload,
      env: envVars ? getPairsObjFromArray(envVars) : undefined,
      session_variables: sessionVars
        ? getPairsObjFromArray(sessionVars)
        : undefined,
      request_transform: transformer,
    },
  };
};

type ValidateTransformOptionsArgsType = {
  version: 1 | 2;
  inputPayloadString: string;
  webhookUrl: string;
  envVarsFromContext?: KeyValuePair[];
  sessionVarsFromContext?: KeyValuePair[];
  transformerBody?: string;
  requestUrl?: string;
  queryParams?: KeyValuePair[];
  isEnvVar?: boolean;
  requestMethod?: Nullable<RequestTransformMethod>;
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
    getTransformer(
      version,
      transformerBody,
      transformerUrl,
      requestMethod,
      queryParams
    ),
    requestPayload,
    webhookUrl,
    sessionVarsFromContext,
    isEnvVar,
    envVarsFromContext
  );

  const options: RequestInit = {
    method: 'POST',
    body: JSON.stringify(finalReqBody),
  };

  return options;
};

const getWordListArray = (mainObj: Record<string, any>) => {
  const uniqueWords = new Set<string>();
  const recursivelyWalkObj = (obj: Record<string, any>) => {
    if (typeof obj === 'object' && obj !== null) {
      Object.entries(obj).forEach(([key, value]) => {
        if (typeof key === 'string') {
          uniqueWords.add(key);
        }
        if (typeof value === 'string') {
          uniqueWords.add(value);
        }
        if (typeof value === 'object' && value != null) {
          recursivelyWalkObj(value);
        }
      });
    }
  };
  recursivelyWalkObj(mainObj);
  return Array.from(uniqueWords);
};

export const getAceCompleterFromString = (jsonString: string) => {
  const jsonObject = isJsonString(jsonString) ? JSON.parse(jsonString) : {};
  const wordListArray = getWordListArray(jsonObject);

  const wordCompleter = {
    getCompletions: (
      editor: any,
      session: any,
      pos: any,
      prefix: string,
      callback: (
        arg1: Nullable<string>,
        arg2: { caption: string; value: string; meta: string }[]
      ) => void
    ) => {
      if (prefix.length === 0) {
        callback(null, []);
        return;
      }
      callback(
        null,
        wordListArray.map(word => {
          return {
            caption: word,
            value: word,
            meta: 'Sample Input',
          };
        })
      );
    },
  };
  return wordCompleter;
};

const getTrimmedRequestUrl = (val: string) => {
  const prefix = `{{$base_url}}`;
  return val.startsWith(prefix) ? val.slice(prefix.length) : val;
};

export const getTransformState = (
  transform: RequestTransform,
  sampleInput: string
): RequestTransformState => ({
  version: transform?.version,
  envVars: getEnvVarsFromLS(),
  sessionVars: getSessionVarsFromLS(),
  requestMethod: transform?.method ?? null,
  requestUrl: transform?.url ? getTrimmedRequestUrl(transform?.url) : '',
  requestUrlError: '',
  requestUrlPreview: '',
  requestQueryParams: getArrayFromServerPairObject(transform?.query_params) ?? [
    { name: '', value: '' },
  ],
  requestAddHeaders: getArrayFromServerPairObject(
    transform?.request_headers?.add_headers
  ) ?? [{ name: '', value: '' }],
  requestBody:
    transform?.version === 1
      ? transform?.body ?? ''
      : transform?.body?.template ?? '',
  requestBodyError: '',
  requestSampleInput: sampleInput,
  requestTransformedBody: '',
  enableRequestBody:
    transform?.version === 2 ? transform?.body?.action === 'transform' : true,
  requestContentType: transform?.content_type ?? defaultRequestContentType,
  isRequestUrlTransform:
    !!transform?.method ||
    !!transform?.url ||
    !isEmpty(transform?.query_params),
  isRequestPayloadTransform: !!transform?.body,
  templatingEngine: transform?.template_engine ?? 'Kriti',
});

export const sidebarNumberStyles =
  '-mb-9 -ml-14 bg-gray-50 text-sm font-medium border border-gray-400 rounded-full flex items-center justify-center h-lg w-lg';

export const inputStyles =
  'block h-10 shadow-sm rounded border-gray-300 hover:border-gray-400 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400';

export const fixedInputStyles =
  'inline-flex items-center h-onput rounded-l text-gray-600 font-semibold px-sm border border-r-0 border-gray-300 bg-gray-50';

export const buttonShadow =
  'bg-gray-50 bg-gradient-to-t from-transparent to-white border border-gray-300 rounded shadow-xs hover:border-gray-400';

export const focusYellowRing =
  'focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-yellow-400';

export const editorDebounceTime = 1000;
