import React, { useState, useEffect, useRef } from 'react';
import { isJsonString } from '@hasura/shared/utils';
import { IAnnotation } from 'react-ace';
import { AceEditor } from '../AceEditor';

export interface JSONEditorProps {
  initData?: string;
  onChange: (v: string) => void;
  data: string;
  minLines?: number;
}

export const JSONEditor: React.FC<JSONEditorProps> = ({
  initData,
  onChange,
  data,
  minLines,
}) => {
  const [value, setValue] = useState(initData || data || '');
  const [annotations, setAnnotations] = useState<IAnnotation[]>([]);
  const prevDataRef = useRef(data);

  useEffect(() => {
    // if the data prop is changed do nothing
    if (prevDataRef.current !== data) return;
    // when state gets new data, trigger parent callback
    if (value !== data) onChange(value);
  }, [value, data]);

  // check and set error message
  useEffect(() => {
    if (isJsonString(value)) {
      setAnnotations([]);
    } else {
      setAnnotations([
        { row: 0, column: 0, text: 'Invalid JSON', type: 'error' },
      ]);
    }
    return () => {
      setAnnotations([]);
    };
  }, [value]);

  useEffect(() => {
    // set data to editor only if the prop has a valid json string
    // setting value from query editor will always have a valid json
    // any invalid json means, the value is set from this component so no need to set that again
    if (isJsonString(data)) setValue(data);
  }, [data]);

  return (
    <AceEditor
      mode="json"
      onChange={setValue}
      height="5em"
      minLines={minLines || 1}
      maxLines={15}
      width="100%"
      showPrintMargin={false}
      value={value}
      annotations={annotations}
      setOptions={{ useWorker: false }}
    />
  );
};
