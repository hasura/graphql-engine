import React, { useState } from 'react';
import { useDebouncedEffect } from '@hasura/shared/hooks';
import CrossIcon from '../../../components/Common/Icons/Cross';
import { isConsoleError, isJsonString } from '@hasura/shared/utils';
import { AceEditor, Text } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

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

  // Mirror the `value` prop into the local editing buffer when it changes
  // upstream. Done during render (the React "adjust state when a prop changes"
  // pattern) rather than in an effect, so the reset is applied in the same
  // commit without an extra render pass.
  const [prevValue, setPrevValue] = useState(value);
  if (value !== prevValue) {
    setPrevValue(value);
    setLocalValue(value);
  }

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
        <Flex className="mb-2" gap="2" align="center">
          <CrossIcon />
          <Text color="red">{error}</Text>
        </Flex>
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
