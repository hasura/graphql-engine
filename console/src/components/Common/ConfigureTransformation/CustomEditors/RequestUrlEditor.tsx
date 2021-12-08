import React, { useState, useEffect } from 'react';
import KeyValueInput from './KeyValueInput';
import { Nullable } from '../../utils/tsUtils';
import { editorDebounceTime, fixedInputStyles, inputStyles } from '../utils';
import { useDebouncedEffect } from '../../../../hooks/useDebounceEffect';
import CrossIcon from '../../Icons/Cross';
import { KeyValuePair } from '../stateDefaults';

type RequestUrlEditorProps = {
  requestUrl: string;
  requestUrlError: string;
  requestUrlPreview: string;
  requestQueryParams: KeyValuePair[];
  requestUrlOnChange: (requestUrl: string) => void;
  requestQueryParamsOnChange: (requestQueryParams: KeyValuePair[]) => void;
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
  const [localError, setLocalError] = useState<Nullable<string>>(
    requestUrlError
  );
  const [localQueryParams, setLocalQueryParams] = useState<KeyValuePair[]>(
    requestQueryParams
  );

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

  const queryParamsOnChangeHandler = (val: KeyValuePair[]) => {
    setLocalQueryParams(val);
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
        <div>
          <label className="block text-gray-600 font-medium mb-xs">Value</label>
        </div>
      </div>
      <div className="grid gap-3 grid-cols-3 mb-sm">
        <KeyValueInput
          pairs={localQueryParams}
          setPairs={queryParamsOnChangeHandler}
        />
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
