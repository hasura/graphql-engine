import React, { useState, useEffect, useRef } from 'react';
import AceEditor from '../../AceEditor/BaseEditor';
import { Nullable } from '../../utils/tsUtils';
import { getAceCompleterFromString, editorDebounceTime } from '../utils';
import { useDebouncedEffect } from '../../../../hooks/useDebounceEffect';
import CrossIcon from '../../Icons/Cross';

type TemplateEditorProps = {
  requestBody: string;
  requestBodyError: string;
  requestSampleInput: string;
  requestBodyOnChange: (requestBody: string) => void;
  requestBodyErrorOnChange: (requestBodyError: string) => void;
  height?: string;
  width?: string;
};

const TemplateEditor: React.FC<TemplateEditorProps> = ({
  requestBody,
  requestBodyError,
  requestSampleInput,
  requestBodyOnChange,
  requestBodyErrorOnChange,
  height,
  width,
}) => {
  const editorRef = useRef<any>();
  const [localValue, setLocalValue] = useState<string>(requestBody);
  const [localError, setLocalError] = useState<Nullable<string>>(
    requestBodyError
  );

  useEffect(() => {
    setLocalValue(requestBody);
  }, [requestBody]);

  useEffect(() => {
    if (requestBodyError) {
      setLocalError(requestBodyError);
    } else {
      setLocalError(null);
    }
  }, [requestBodyError]);

  useEffect(() => {
    const sampleInputWordCompleter = getAceCompleterFromString(
      requestSampleInput
    );
    if (
      editorRef?.current?.editor?.completers &&
      Array.isArray(editorRef?.current?.editor?.completers)
    ) {
      editorRef.current.editor.completers = [sampleInputWordCompleter];
    }
  }, [requestSampleInput]);

  useDebouncedEffect(
    () => {
      requestBodyErrorOnChange('');
      requestBodyOnChange(localValue);
    },
    editorDebounceTime,
    [localValue]
  );

  const onChangeHandler = (val: string) => {
    setLocalError(null);
    setLocalValue(val);
  };

  return (
    <>
      {localError && (
        <div className="mb-sm" data-test="transform-requestBody-error">
          <CrossIcon />
          <span className="text-red-500 ml-sm">{localError}</span>
        </div>
      )}
      <AceEditor
        name="temp-editor"
        mode="json"
        editorRef={editorRef}
        value={localValue}
        onChange={onChangeHandler}
        showPrintMargin={false}
        height={height || '200px'}
        width={width || '100%'}
        fontSize="12px"
        setOptions={{
          enableBasicAutocompletion: true,
          enableLiveAutocompletion: true,
        }}
      />
    </>
  );
};

export default TemplateEditor;
