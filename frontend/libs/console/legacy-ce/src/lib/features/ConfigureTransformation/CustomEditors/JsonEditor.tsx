import React, { useEffect, useState } from 'react';
import { useDebouncedEffect } from '@hasura/shared/hooks';
import CrossIcon from '../../../components/Common/Icons/Cross';
import { isConsoleError, isJsonString } from '@hasura/shared/utils';
import { AceEditor } from '@hasura/shared/ui';

type JsonEditorProps = {
  value: string;
  onChange: (value: string) => void;
  fontSize?: string;
  height?: string;
  width?: string;
};

const JsonEditor: React.FC<JsonEditorProps> = ({
  value,
  onChange,
  fontSize,
  height,
  width,
}) => {
  const [localValue, setLocalValue] = useState(value);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    setLocalValue(value);
  }, [value]);

  const onChangeHandler = (val: string) => {
    setLocalValue(val);
    setError(null);
    try {
      JSON.parse(val);
    } catch (e) {
      if (isConsoleError(e)) {
        setError(e.message);
      }
    }
  };

  useDebouncedEffect(
    () => {
      if (isJsonString(localValue)) {
        onChange(localValue);
      }
    },
    // large debounce as it will trigger calculation of autocompleter for Request body (and will trigger validate api)
    3500,
    [localValue],
  );

  return (
    <>
      {error && (
        <div className="mb-2">
          <CrossIcon />
          <span className="text-red-500 ml-2">{error}</span>
        </div>
      )}
      <AceEditor
        name="json-editor"
        mode="json"
        value={localValue}
        onChange={onChangeHandler}
        fontSize={fontSize || '12px'}
        height={height || '200px'}
        width={width || '100%'}
        showPrintMargin={false}
        setOptions={{ useWorker: false }}
      />
    </>
  );
};

export default JsonEditor;
