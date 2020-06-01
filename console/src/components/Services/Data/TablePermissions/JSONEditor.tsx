import React, { useState, useEffect } from 'react';
import AceEditor, { IAnnotation } from 'react-ace';
import { isJsonString } from '../../../Common/utils/jsUtils';

export interface JSONEditorProps {
  initData: string;
  onChange: (v: string) => void;
  data: string;
}

const JSONEditor: React.FC<JSONEditorProps> = ({
  initData,
  onChange,
  data,
}) => {
  const [value, setValue] = useState(initData || data || '');
  const [annotations, setAnnotations] = useState<IAnnotation[]>([]);
  useEffect(() => {
    if (value !== data) onChange(value);
  }, [value]);

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
      theme="github"
      height="5em"
      maxLines={15}
      width="100%"
      showPrintMargin={false}
      value={value}
      annotations={annotations}
      debounceChangePeriod={200}
    />
  );
};

export default JSONEditor;
