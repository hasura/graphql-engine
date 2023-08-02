import React, { useState, useEffect } from 'react';
import { QueryParams } from '../../../../metadata/types';
import KeyValueInput from './KeyValueInput';
import { Nullable } from '../../utils/tsUtils';
import {
  editorDebounceTime,
  fixedInputStyles,
  focusYellowRing,
  inputStyles,
} from '../utils';
import { useDebouncedEffect } from '../../../../hooks/useDebounceEffect';
import CrossIcon from '../../Icons/Cross';
import AceEditor from '../../../Common/AceEditor/BaseEditor';

type RequestUrlEditorProps = {
  requestUrl: string;
  requestUrlError: string;
  requestUrlPreview: string;
  requestQueryParams: QueryParams;
  requestUrlOnChange: (requestUrl: string) => void;
  requestQueryParamsOnChange: (requestQueryParams: QueryParams) => void;
};

type QueryParmsOptions = {
  value: 'key-value' | 'url-string';
  label: string;
};

const RequestUrlEditor: React.FC<RequestUrlEditorProps> = ({
  requestUrl,
  requestUrlError,
  requestUrlPreview,
  requestQueryParams,
  requestUrlOnChange,
  requestQueryParamsOnChange,
}) => {
  const [localUrl, setLocalUrl] = useState<string>(requestUrl);
  const [queryParamsType, setQueryParamstype] = useState<
    QueryParmsOptions['value']
  >(typeof requestQueryParams === 'string' ? 'url-string' : 'key-value');

  const [keyValueQueryParams, setKeyValueQueryParams] = useState(
    typeof requestQueryParams !== 'string'
      ? requestQueryParams
      : [{ name: '', value: '' }]
  );
  const [stringQueryParams, setStringQueryParams] = useState(
    typeof requestQueryParams === 'string' ? requestQueryParams : ''
  );
  const [localError, setLocalError] =
    useState<Nullable<string>>(requestUrlError);
  const [localQueryParams, setLocalQueryParams] =
    useState<QueryParams>(requestQueryParams);

  useEffect(() => {
    setQueryParamstype(
      typeof requestQueryParams === 'string' ? 'url-string' : 'key-value'
    );
    setKeyValueQueryParams(
      typeof requestQueryParams !== 'string'
        ? requestQueryParams
        : keyValueQueryParams
    );
    setStringQueryParams(
      typeof requestQueryParams === 'string'
        ? requestQueryParams
        : stringQueryParams
    );
  }, [requestQueryParams]);

  const executionOptions: QueryParmsOptions[] = [
    {
      value: 'key-value',
      label: 'Key-Value',
    },
    {
      value: 'url-string',
      label: 'URL string',
    },
  ];

  useEffect(() => {
    setLocalUrl(requestUrl);
  }, [requestUrl]);

  useEffect(() => {
    if (requestUrlError) {
      setLocalError(requestUrlError);
    } else {
      setLocalError(null);
    }
  }, [requestUrlError]);

  useEffect(() => {
    setLocalQueryParams(requestQueryParams);
  }, [requestQueryParams]);

  useDebouncedEffect(
    () => {
      requestUrlOnChange(localUrl);
    },
    editorDebounceTime,
    [localUrl]
  );

  useDebouncedEffect(
    () => {
      requestQueryParamsOnChange(localQueryParams);
    },
    editorDebounceTime,
    [localQueryParams]
  );

  const urlOnChangeHandler = (val: string) => {
    setLocalUrl(val);
  };

  const queryParamsOnChangeHandler = (val: QueryParams) => {
    setLocalQueryParams(val);
  };

  const editorOptions = {
    minLines: 10,
    maxLines: 10,
    showLineNumbers: true,
    useSoftTabs: true,
  };

  return (
    <>
      <div className="grid gap-3 grid-cols-3 mb-sm">
        <div className="col-span-2">
          <div className="flex shadow-sm rounded w-full">
            <span className={fixedInputStyles}>
              &#123;&#123;$base_url&#125;&#125;
            </span>
            <input
              type="text"
              name="request_url"
              id="request_url"
              className={`w-full ${inputStyles}`}
              placeholder="URL Template (Optional)..."
              value={localUrl}
              onChange={e => urlOnChangeHandler(e.target.value)}
              data-test="transform-requestUrl"
            />
          </div>
        </div>
      </div>
      <div className="grid gap-3 grid-cols-3">
        <div>
          <label className="block text-gray-600 font-medium mb-xs">
            Query Params
          </label>
        </div>
      </div>
      {executionOptions.map((option, i) => (
        <div className="inline-flex items-center mr-md">
          <input
            key={i}
            id={option.value}
            name="queryParamType"
            type="radio"
            value={option.value}
            checked={queryParamsType === option.value}
            onChange={() => {
              setQueryParamstype(option.value);
              queryParamsOnChangeHandler(
                option.value === 'key-value'
                  ? keyValueQueryParams
                  : stringQueryParams
              );
            }}
            className={`mb-xs ${focusYellowRing}`}
          />
          <label className="ml-sm pt-sm font-normal" htmlFor={option.value}>
            {option.label}
          </label>
        </div>
      ))}
      <div className="grid gap-3 grid-cols-3 mb-sm">
        {queryParamsType === 'key-value' ? (
          <KeyValueInput
            pairs={keyValueQueryParams}
            setPairs={pairs => {
              queryParamsOnChangeHandler(pairs);
              setKeyValueQueryParams(pairs);
            }}
            testId="query-params"
          />
        ) : (
          <AceEditor
            name="sdl-editor"
            value={stringQueryParams}
            onChange={value => {
              queryParamsOnChangeHandler(value);
              setStringQueryParams(value);
            }}
            placeholder={`You can also use Kriti Template here to customise the query parameter string.

e.g. {{concat(["userId=", $session_variables["x-hasura-user-id"]])}}`}
            height="200px"
            mode="graphqlschema"
            width="610px"
            showPrintMargin={false}
            setOptions={editorOptions}
            className="block relative inset-0 input shadow-sm rounded border border-gray-300 focus-within:outline-0 focus-within:ring-2 focus-within:ring-yellow-200 focus-within:border-yellow-400 placeholder-gray-500 block hover:border-gray-200"
          />
        )}
      </div>
      <div className="grid gap-3 grid-cols-3 mb-sm">
        <div className="col-span-2">
          <label
            htmlFor="request_url_preview"
            className="block text-gray-600 font-medium mb-xs"
          >
            Preview
          </label>
          <input
            disabled
            type="text"
            name="request_url_preview"
            id="request_url_preview"
            className="w-full block cursor-not-allowed rounded border-gray-200 bg-gray-200"
            data-test="transform-requestUrl-preview"
            value={requestUrlPreview}
          />
        </div>
      </div>
      {localError ? (
        <div className="mb-sm" data-test="transform-requestUrl-error">
          <CrossIcon />
          <span className="text-red-500 ml-sm">{localError}</span>
        </div>
      ) : null}
    </>
  );
};

export default RequestUrlEditor;
