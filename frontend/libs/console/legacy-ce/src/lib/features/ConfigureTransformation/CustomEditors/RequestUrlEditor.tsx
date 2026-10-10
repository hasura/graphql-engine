import React, { useState, useEffect } from 'react';
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

  useEffect(() => {
    // eslint-disable-next-line react-hooks/set-state-in-effect
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
  }, [requestQueryParams]);

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
        full
      />
      <Flex direction="column" gap="2">
        <FieldLabel label="Query Params" />
        <RadioGroup
          orientation="horizontal"
          options={executionOptions}
          value={queryParamsType}
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
          className="w-full block cursor-not-allowed rounded border-gray-200 bg-gray-200"
          data-test="transform-requestUrl-preview"
          value={requestUrlPreview}
        />
      </Flex>
      {localError ? (
        <IndicatorCard status="negative">{localError}</IndicatorCard>
      ) : null}
    </Flex>
  );
};

export default RequestUrlEditor;
