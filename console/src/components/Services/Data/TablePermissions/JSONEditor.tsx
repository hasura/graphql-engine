import React, { useState, useEffect, useCallback } from 'react';
import AceEditor, { IAnnotation } from 'react-ace';
import { isJsonString } from '../../../Common/utils/jsUtils';
import { usePrevious } from '../../../../hooks/usePrevious';

export interface JSONEditorProps {
  initData: string;
  onChange: (v: string) => void;
  data: string;
  minLines?: number;
}

const JSONEditor: React.FC<JSONEditorProps> = ({
  initData,
  onChange,
  data,
  minLines,
}) => {
  const [value, setValue] = useState(initData || data || '');
  const [annotations, setAnnotations] = useState<IAnnotation[]>([]);
  const prevData = usePrevious<string>(data);

  useEffect(() => {
    // if the data prop is changed do nothing
    if (prevData !== data) return;
    // when state gets new data, trigger parent callback
    if (value !== data) onChange(value);
  }, [value, data, prevData]);

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

  const onEditorValueChange = useCallback(newVal => setValue(newVal), [
    setValue,
  ]);

  return (
    <AceEditor
      mode="json"
      onChange={onEditorValueChange}
      theme="github"
      height="5em"
      minLines={minLines || 1}
      maxLines={15}
      width="100%"
      showPrintMargin={false}
      value={value}
      annotations={annotations}
    />
  );
};

export default JSONEditor;
