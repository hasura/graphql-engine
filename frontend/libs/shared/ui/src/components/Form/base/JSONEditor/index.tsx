import React, { useState, useEffect, useRef } from 'react';
import { isJsonString } from '@hasura/shared/utils';
import { AceEditor, AceEditorProps } from '../AceEditor';
import { getJsonAnnotations } from './getJsonErrorAnnotation';

export {
  getJsonAnnotations,
  getJsonErrorAnnotation,
} from './getJsonErrorAnnotation';

export type JSONEditorProps = Omit<
  AceEditorProps,
  'mode' | 'value' | 'onChange' | 'annotations'
> & {
  initData?: string;
  onChange: (v: string) => void;
  data: string;
  /** don't mark a blank editor as invalid */
  allowEmpty?: boolean;
};

export const JSONEditor: React.FC<JSONEditorProps> = ({
  initData,
  onChange,
  data,
  allowEmpty,
  minLines,
  invalid,
  ...props
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
  const annotations = React.useMemo(
    () => getJsonAnnotations(value, { allowEmpty }),
    [value, allowEmpty],
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
      height="5em"
      maxLines={15}
      width="100%"
      showPrintMargin={false}
      setOptions={{ useWorker: false }}
      {...props}
      mode="json"
      onChange={setValue}
      minLines={minLines || 1}
      value={value}
      annotations={annotations}
      invalid={invalid ?? annotations.length > 0}
    />
  );
};
