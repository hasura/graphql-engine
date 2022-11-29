import React, { useState, useEffect, useRef } from 'react';
import AceEditor from '../../AceEditor/BaseEditor';
import { Nullable } from '../../utils/tsUtils';
import { getAceCompleterFromString, editorDebounceTime } from '../utils';
import { useDebouncedEffect } from '../../../../hooks/useDebounceEffect';
import CrossIcon from '../../Icons/Cross';
import { RequestTransformStateBody } from '../stateDefaults';

type TemplateEditorProps = {
  requestBody: RequestTransformStateBody;
  requestBodyError?: string;
  requestSampleInput?: string;
  requestBodyOnChange: (requestBody: RequestTransformStateBody) => void;
  height?: string;
  width?: string;
};

const TemplateEditor: React.FC<TemplateEditorProps> = ({
  requestBody,
  requestBodyError,
  requestSampleInput,
  requestBodyOnChange,
  height,
  width,
}) => {
  const editorRef = useRef<any>();
  const [localValue, setLocalValue] = useState<string>(
    requestBody.template ?? ''
  );
  const [localError, setLocalError] =
    useState<Nullable<string>>(requestBodyError);

  useEffect(() => {
    setLocalValue(requestBody.template ?? '');
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
      requestSampleInput || ''
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
      requestBodyOnChange({ ...requestBody, template: localValue });
    },
    editorDebounceTime,
    [localValue]
  );

  const onChangeHandler = (val: string) => {
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
          useWorker: false,
        }}
      />
    </>
  );
};

export default TemplateEditor;
