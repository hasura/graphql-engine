import React, { useState } from 'react';
import { Flex } from '@radix-ui/themes';
import KeyValueInput from './KeyValueInput';
import { editorDebounceTime } from '../utils';
import { useDebouncedEffect } from '@hasura/shared/hooks';
import {
  AceEditor,
  FieldLabel,
  IndicatorCard,
  Input,
  RadioGroup,
} from '@hasura/shared/ui';
import { QueryParams } from '../stateDefaults';

type RequestUrlEditorProps = {
  requestUrl: string;
  requestUrlError: string;
  requestUrlPreview: string;
  requestQueryParams: QueryParams;
  requestUrlOnChange: (requestUrl: string) => void;
  requestQueryParamsOnChange: (requestQueryParams: QueryParams) => void;
  disabled?: boolean;
};

type QueryParmsOptions = {
  value: 'key-value' | 'url-string';
  label: string;
};

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

const RequestUrlEditor: React.FC<RequestUrlEditorProps> = ({
  requestUrl,
  requestUrlError,
  requestUrlPreview,
  requestQueryParams,
  requestUrlOnChange,
  requestQueryParamsOnChange,
  disabled,
}) => {
  const [localUrl, setLocalUrl] = useState<string>(requestUrl);
  const [queryParamsType, setQueryParamstype] = useState<
    QueryParmsOptions['value']
  >(typeof requestQueryParams === 'string' ? 'url-string' : 'key-value');

  const [keyValueQueryParams, setKeyValueQueryParams] = useState(
    typeof requestQueryParams !== 'string'
      ? requestQueryParams
      : [{ name: '', value: '' }],
  );
  const [stringQueryParams, setStringQueryParams] = useState(
    typeof requestQueryParams === 'string' ? requestQueryParams : '',
  );
  const [localError, setLocalError] = useState<string | null>(requestUrlError);
  const [localQueryParams, setLocalQueryParams] =
    useState<QueryParams>(requestQueryParams);

  // Re-sync the local editing buffers whenever the upstream props change. These
  // are done during render (the React "adjust state when a prop changes"
  // pattern) rather than in effects, so the resets land in the same commit with
  // no extra render pass. The guards (`prop !== prevProp`) make them run exactly
  // once per upstream change, exactly like the previous dependency-array effects.
  const [prevRequestQueryParams, setPrevRequestQueryParams] =
    useState(requestQueryParams);
  if (requestQueryParams !== prevRequestQueryParams) {
    setPrevRequestQueryParams(requestQueryParams);
    setQueryParamstype(
      typeof requestQueryParams === 'string' ? 'url-string' : 'key-value',
    );
    setKeyValueQueryParams(
      typeof requestQueryParams !== 'string'
        ? requestQueryParams
        : keyValueQueryParams,
    );
    setStringQueryParams(
      typeof requestQueryParams === 'string'
        ? requestQueryParams
        : stringQueryParams,
    );
    setLocalQueryParams(requestQueryParams);
  }

  const [prevRequestUrl, setPrevRequestUrl] = useState(requestUrl);
  if (requestUrl !== prevRequestUrl) {
    setPrevRequestUrl(requestUrl);
    setLocalUrl(requestUrl);
  }

  const [prevRequestUrlError, setPrevRequestUrlError] =
    useState(requestUrlError);
  if (requestUrlError !== prevRequestUrlError) {
    setPrevRequestUrlError(requestUrlError);
    setLocalError(requestUrlError ? requestUrlError : null);
  }

  useDebouncedEffect(
    () => {
      requestUrlOnChange(localUrl);
    },
    editorDebounceTime,
    [localUrl],
  );

  useDebouncedEffect(
    () => {
      requestQueryParamsOnChange(localQueryParams);
    },
    editorDebounceTime,
    [localQueryParams],
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
    readOnly: disabled,
  };

  return (
    <Flex direction="column" gap="2">
      <Input
        type="text"
        name="request_url"
        id="request_url"
        prependLabel="{{$base_url}}"
        placeholder="URL Template (Optional)..."
        value={localUrl}
        onChange={(e) => urlOnChangeHandler(e.target.value)}
        data-test="transform-requestUrl"
        disabled={disabled}
        full
      />
      <Flex direction="column" gap="2">
        <FieldLabel label="Query Params" />
        <RadioGroup
          orientation="horizontal"
          options={executionOptions}
          value={queryParamsType}
          disabled={disabled}
          onChange={(value) => {
            setQueryParamstype(value as 'key-value' | 'url-string');
            queryParamsOnChangeHandler(
              value === 'key-value' ? keyValueQueryParams : stringQueryParams,
            );
          }}
        />
      </Flex>
      <div className="mb-2">
        {queryParamsType === 'key-value' ? (
          <KeyValueInput
            pairs={keyValueQueryParams}
            setPairs={(pairs) => {
              queryParamsOnChangeHandler(pairs);
              setKeyValueQueryParams(pairs);
            }}
            testId="query-params"
            disabled={disabled}
          />
        ) : (
          <AceEditor
            name="sdl-editor"
            value={stringQueryParams}
            onChange={(value) => {
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
          />
        )}
      </div>
      <Flex direction="column" gap="2">
        <FieldLabel id="request_url_preview" label="Preview" />
        <Input
          disabled
          type="text"
          name="request_url_preview"
          id="request_url_preview"
          className="block cursor-not-allowed"
          data-test="transform-requestUrl-preview"
          value={requestUrlPreview}
          full
        />
      </Flex>
      {localError ? (
        <IndicatorCard status="negative">{localError}</IndicatorCard>
      ) : null}
    </Flex>
  );
};

export default RequestUrlEditor;
