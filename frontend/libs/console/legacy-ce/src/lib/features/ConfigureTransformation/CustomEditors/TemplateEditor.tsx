import React, { useState, useEffect, useRef } from 'react';
import { getAceCompleterFromString, editorDebounceTime } from '../utils';
import { useDebouncedEffect } from '@hasura/shared/hooks';
import { RequestTransformStateBody } from '../stateDefaults';
import { AceEditor, AceEditorRef, IndicatorCard } from '@hasura/shared/ui';

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
  const editorRef = useRef<AceEditorRef>(undefined);
  const [localValue, setLocalValue] = useState<string>(
    requestBody.template ?? '',
  );
  const [localError, setLocalError] = useState<string | null | undefined>(
    requestBodyError,
  );

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
      requestSampleInput || '',
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
    [localValue],
  );

  const onChangeHandler = (val: string) => {
    setLocalValue(val);
  };

  return (
    <>
      {localError && (
        <IndicatorCard status="negative" showIcon className="mb-2">
          {localError}
        </IndicatorCard>
      )}
      <AceEditor
        name="temp-editor"
        mode="json"
        ref={(editor) => {
          if (!editor) {
            return;
          }

          editorRef.current = editor;
        }}
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
