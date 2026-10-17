import React, { useState, useEffect, useRef } from 'react';
import { isJsonString } from '@hasura/shared/utils';
import { IAnnotation } from 'react-ace';
import { AceEditor } from '../AceEditor';

export interface JSONEditorProps {
  initData?: string;
  onChange: (v: string) => void;
  data: string;
  minLines?: number;
  disabled?: boolean;
}

export const JSONEditor: React.FC<JSONEditorProps> = ({
  initData,
  onChange,
  data,
  minLines,
  disabled,
}) => {
  const [value, setValue] = useState(initData || data || '');
  const prevDataRef = useRef(data);

  // Keep the latest `onChange` in a ref so the effect below can notify the
  // parent with the current callback without having to re-run (and without
  // forcing parents to memoize `onChange`).
  const onChangeRef = useRef(onChange);
  useEffect(() => {
    onChangeRef.current = onChange;
  });

  useEffect(() => {
    // if the data prop is changed do nothing (but remember it, so later edits
    // are still propagated to the parent)
    if (prevDataRef.current !== data) {
      prevDataRef.current = data;
      return;
    }
    // when state gets new data, trigger parent callback
    if (value !== data) onChangeRef.current(value);
  }, [value, data]);

  // The error annotation is fully derived from the current value, so compute it
  // during render instead of mirroring it into state via an effect.
  const annotations = React.useMemo<IAnnotation[]>(
    () =>
      isJsonString(value)
        ? []
        : [{ row: 0, column: 0, text: 'Invalid JSON', type: 'error' }],
    [value],
  );

  // Sync the editor value when the `data` prop changes to a valid json string.
  // Done during render (guarded by the previous prop) rather than in an effect
  // to avoid an extra commit.
  // setting value from query editor will always have a valid json
  // any invalid json means, the value is set from this component so no need to set that again
  const [prevData, setPrevData] = useState(data);
  if (data !== prevData) {
    setPrevData(data);
    if (isJsonString(data)) setValue(data);
  }

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
      disabled={disabled}
      setOptions={{ useWorker: false }}
    />
  );
};
