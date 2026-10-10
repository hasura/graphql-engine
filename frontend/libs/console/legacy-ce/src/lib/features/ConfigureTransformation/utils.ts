import { getLSItem, setLSItem } from '@hasura/shared/utils';
import {
  defaultRequestContentType,
  GraphiQlHeader,
  QueryParams,
  RequestTransformState,
  RequestTransformStateBody,
  ResponseTransformState,
  ResponseTransformStateBody,
} from './stateDefaults';
import { isEmpty, isJsonString } from '@hasura/shared/utils';
import {
  requestBodyActionState,
  responseBodyActionState,
} from './requestTransformState';
import {
  ActionRequestTransform,
  RequestTransformContentType,
  RequestTransformMethod,
  ResponseTransform,
} from '@hasura/shared/types';
import { EventRequestTransform } from '@hasura/shared/types';
import { LS_KEYS, NameValue } from '@hasura/shared/types';

type Nullable<T> = T | null | undefined;

export const getPairsObjFromArray = (values: NameValue[] | undefined) => {
  if (!values?.length) {
    return undefined;
  }

  return values.reduce(
    (acc, item) => {
      acc[item.name] = item.value;
      return acc;
    },
    {} as Record<string, string>,
  );
};

export const addPlaceholderValue = (pairs: NameValue[]) => {
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
  let sessionVars: NameValue[] = [];
  if (Array.isArray(headers)) {
    sessionVars = headers
      .filter(
        (header: GraphiQlHeader) =>
          header.isActive && header.key?.toLowerCase().startsWith('x-hasura'),
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
  const envVars: NameValue[] = isJsonString(lsEnvString)
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

export const setEnvVarsToLS = (envVars: NameValue[]) => {
  const validEnvVars = envVars.filter(
    (e) => !isEmpty(e.name) && !isEmpty(e.value),
  );
  setLSItem(LS_KEYS.webhookTransformEnvVars, JSON.stringify(validEnvVars));
};

export const getArrayFromServerPairObject = (
  pairs: Nullable<Record<string, string>> | string,
): NameValue[] => {
  const transformArray: NameValue[] = [];
  if (pairs && Object.keys(pairs as NameValue).length !== 0) {
    Object.entries(pairs as NameValue).forEach(([key, value]) => {
      transformArray.push({ name: key, value });
    });
  }
  transformArray.push({ name: '', value: '' });
  return transformArray;
};

const getUrlWithBasePrefix = (val?: string) => {
  return val ? `{{$base_url}}${val}` : undefined;
};

const getTransformBodyServer = (reqBody: RequestTransformStateBody) => {
  if (reqBody.action === requestBodyActionState.remove)
    return { action: reqBody.action };
  else if (reqBody.action === requestBodyActionState.transformApplicationJson)
    return { action: reqBody.action, template: reqBody.template };
  return {
    action: reqBody.action,
    form_template: getPairsObjFromArray(reqBody.form_template ?? []),
  };
};

export const getRequestTransformObject = (
  transformState: RequestTransformState,
) => {
  const isRequestUrlTransform = transformState.isRequestUrlTransform;
  const isRequestPayloadTransform = transformState.isRequestPayloadTransform;

  if (!isRequestUrlTransform && !isRequestPayloadTransform) return null;

  let obj: ActionRequestTransform = {
    version: 2,
    template_engine: transformState.templatingEngine,
  };

  if (isRequestUrlTransform) {
    obj = {
      ...obj,
      method: transformState.requestMethod,
      url: getUrlWithBasePrefix(transformState.requestUrl),
      query_params:
        typeof transformState.requestQueryParams !== 'string'
          ? // Drop the empty-name placeholder row the key/value editor keeps, so an
            // untouched transform serialises `{}` instead of a junk `{ "": "" }`.
            (getPairsObjFromArray(
              transformState.requestQueryParams.filter(
                (pair) => pair.name !== '',
              ),
            ) ?? {})
          : transformState.requestQueryParams,
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
      body: getTransformBodyServer(transformState.requestBody),
    };
    if (
      transformState.requestBody.action ===
      requestBodyActionState.transformFormUrlEncoded
    ) {
      obj = {
        ...obj,
        request_headers: {
          remove_headers: ['content-type'],
          add_headers: {
            'content-type': 'application/x-www-form-urlencoded',
          },
        },
      };
    }
  }

  return obj;
};

export const getResponseTransformObject = (
  responseTransformState: ResponseTransformState,
) => {
  const isResponsePayloadTransform =
    responseTransformState.isResponsePayloadTransform;

  if (!isResponsePayloadTransform) return null;

  const obj: ResponseTransform = {
    version: 2,
    body: getTransformBodyServer(responseTransformState.responseBody),
    template_engine: responseTransformState.templatingEngine,
  };

  return obj;
};

const getErrorFromCode = (data: Record<string, any>) => {
  const errorCode = data.code ? data.code : '';
  const errorMsg = data.error ? data.error : '';
  return `${errorCode}: ${errorMsg}`;
};

const getErrorFromBody = (errorObj: Record<string, any>) => {
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
  requestData: Record<string, any> | Record<string, any>[],
  setError: (error: string) => void,
  setUrl?: (data: string) => void,
  setBody?: (data: string) => void,
) => {
  if (Array.isArray(requestData)) {
    const errorMessage = getErrorFromBody(requestData[0]);
    setError(errorMessage);
  } else if (requestData?.code) {
    const errorMessage = getErrorFromCode(requestData);
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

export type ValidateTransformOptionsArgsType = {
  version: '1' | '2';
  inputPayloadString: string;
  webhookUrl: string;
  envVarsFromContext?: NameValue[];
  sessionVarsFromContext?: NameValue[];
  transformerBody?: RequestTransformStateBody;
  requestUrl?: string;
  queryParams?: QueryParams;
  isEnvVar?: boolean;
  requestMethod?: Nullable<RequestTransformMethod>;
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
        arg2: { caption: string; value: string; meta: string }[],
      ) => void,
    ) => {
      if (prefix.length === 0) {
        callback(null, []);
        return;
      }
      callback(
        null,
        wordListArray.map((word) => {
          return {
            caption: word,
            value: word,
            meta: 'Sample Input',
          };
        }),
      );
    },
  };
  return wordCompleter;
};

const getTrimmedRequestUrl = (val: string) => {
  const prefix = `{{$base_url}}`;
  return val.startsWith(prefix) ? val.slice(prefix.length) : val;
};

const getRequestTransformBody = (
  transform: ActionRequestTransform,
): RequestTransformStateBody => {
  if (transform.body) {
    return transform.version === 1
      ? {
          action: requestBodyActionState.transformApplicationJson,
          template: transform?.body ?? '',
        }
      : {
          ...transform.body,
          form_template: getArrayFromServerPairObject(
            transform.body?.form_template,
          ),
        };
  }
  return {
    action: requestBodyActionState.transformApplicationJson,
    template: '',
  };
};

const getResponseTransformBody = (
  responseTransform: ResponseTransform,
): ResponseTransformStateBody => {
  return {
    action: responseBodyActionState.transformApplicationJson,
    template: responseTransform.body?.template ?? '',
  };
};

export const getTransformState = (
  transform: EventRequestTransform,
  sampleInput: string,
): RequestTransformState => ({
  version: (transform?.version || 2).toString() as '1' | '2',
  envVars: getEnvVarsFromLS(),
  sessionVars: getSessionVarsFromLS(),
  requestMethod: transform?.method ?? null,
  requestUrl: transform?.url ? getTrimmedRequestUrl(transform?.url) : '',
  requestUrlError: '',
  requestUrlPreview: '',
  requestQueryParams:
    typeof transform?.query_params === 'string'
      ? transform.query_params
      : getArrayFromServerPairObject(transform?.query_params),
  requestAddHeaders: getArrayFromServerPairObject(
    transform?.request_headers?.add_headers,
  ) ?? [{ name: '', value: '' }],
  requestBody: getRequestTransformBody(transform),
  requestBodyError: '',
  requestSampleInput: sampleInput,
  requestTransformedBody: '',
  requestContentType:
    (transform?.content_type as RequestTransformContentType) ??
    defaultRequestContentType,
  isRequestUrlTransform:
    !!transform?.method ||
    !!transform?.url ||
    !isEmpty(transform?.query_params),
  isRequestPayloadTransform: !!transform?.body,
  templatingEngine: transform?.template_engine ?? 'Kriti',
});

export const getResponseTransformState = (
  responseTransform: ResponseTransform,
): ResponseTransformState => ({
  version: responseTransform?.version,
  templatingEngine: responseTransform?.template_engine ?? 'Kriti',
  isResponsePayloadTransform: !!responseTransform?.body,
  responseBody: getResponseTransformBody(responseTransform),
});

export const fixedInputStyles =
  'inline-flex items-center h-onput rounded-l text-gray-600 font-semibold px-sm border border-r-0 border-gray-300 bg-gray-50';

export const buttonShadow =
  'bg-gray-50 bg-linear-to-t from-transparent to-white border border-gray-300 rounded shadow-xs hover:border-gray-400';

export const editorDebounceTime = 1000;
