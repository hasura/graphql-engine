import { useState } from 'react';
import { JSONEditor } from '@hasura/shared/ui';
import { isJsonString } from '@hasura/shared/utils';

import { ExpandToggle, expandedEditorTw } from './ExpandToggle';
import { formatJsonInput } from './formatJsonInput';
import { TextInput, TextInputProps } from './TextInput';

export type JsonInputProps = {
  initialValue?: string;
} & TextInputProps;

/** A one-line JSON input that expands into a validating JSON editor. */
export const JsonInput: React.FC<JsonInputProps> = ({
  onChange,
  onInput,
  initialValue,
  isInvalid,
  ...restProps
}) => {
  const [isEditorActive, setEditorActive] = useState(false);

  const [value, setValue] = useState(() =>
    formatJsonInput(initialValue ?? '', false),
  );

  // a blank value leaves the column unset, so it isn't an error
  const isInvalidJson = value.trim() !== '' && !isJsonString(value);

  // pretty-print in the editor and compact it back on collapse
  const toggleEditor = () => {
    const expanded = !isEditorActive;
    setValue((current) => formatJsonInput(current, expanded));
    setEditorActive(expanded);
  };

  const onChangeHandler = (e: React.ChangeEvent<HTMLInputElement>) => {
    setValue(e.target.value);
    onChange?.(e);
  };

  const onEditorChange = (newText: string) => {
    setValue(newText);
    const event = {
      target: { value: newText },
    } as unknown as React.ChangeEvent<HTMLInputElement>;
    onChange?.(event);
    onInput?.(event);
  };

  return (
    <div className="relative block w-full">
      <ExpandToggle expanded={isEditorActive} onToggle={toggleEditor} />
      {!isEditorActive ? (
        <TextInput
          {...restProps}
          onChange={onChangeHandler}
          onInput={onInput}
          value={value}
          isInvalid={isInvalid || isInvalidJson}
          className="pr-8"
        />
      ) : (
        <JSONEditor
          className={expandedEditorTw}
          data={value}
          onChange={onEditorChange}
          allowEmpty
          minLines={10}
          maxLines={30}
          focus
        />
      )}
    </div>
  );
};
