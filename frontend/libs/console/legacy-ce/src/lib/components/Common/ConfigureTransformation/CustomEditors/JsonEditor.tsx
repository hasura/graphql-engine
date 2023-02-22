import React, { useEffect, useState } from 'react';
import { useDebouncedEffect } from '../../../../hooks/useDebounceEffect';
import AceEditor from '../../AceEditor/BaseEditor';
import CrossIcon from '../../Icons/Cross';
import { isConsoleError, isJsonString } from '../../utils/jsUtils';
import { Nullable } from '../../utils/tsUtils';

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
  const [error, setError] = useState<Nullable<string>>(null);

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
    [localValue]
  );

  return (
    <>
      {error && (
        <div className="mb-sm">
          <CrossIcon />
          <span className="text-red-500 ml-sm">{error}</span>
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
