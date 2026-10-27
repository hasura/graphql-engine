import { useState } from 'react';
import { AceEditor } from '@hasura/shared/ui';

import { ExpandToggle, expandedEditorTw } from './ExpandToggle';
import { TextInput, TextInputProps } from './TextInput';

export type ExpandableTextInputProps = {
  mode?: 'normal' | 'text' | 'markdown' | 'html';
  initialValue?: string;
} & TextInputProps;

export const ExpandableTextInput: React.FC<ExpandableTextInputProps> = ({
  mode = 'normal',
  onChange,
  onInput,
  initialValue,
  ...restProps
}) => {
  const [isBigEditorActive, setBigEditorActive] = useState(false);

  const [value, setValue] = useState(initialValue ?? '');

  const onChangeHandler = (e: React.ChangeEvent<HTMLInputElement>) => {
    setValue(e.target.value);
    if (onChange) {
      onChange(e);
    }
  };

  const onInputHandler = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (onInput) {
      onInput(e);
    }
  };

  const onAceEditorChange = (newText: string) => {
    setValue(newText);
    if (onChange) {
      onChange({
        target: { value: newText },
      } as unknown as React.ChangeEvent<HTMLInputElement>);
    }

    if (onInput) {
      onInput({
        target: { value: newText },
      } as unknown as React.ChangeEvent<HTMLInputElement>);
    }
  };

  return (
    <div className="relative block w-full">
      <ExpandToggle
        expanded={isBigEditorActive}
        onToggle={() => setBigEditorActive(!isBigEditorActive)}
      />
      {!isBigEditorActive ? (
        <TextInput
          {...restProps}
          onChange={onChangeHandler}
          onInput={onInputHandler}
          value={value}
          className="pr-8"
        />
      ) : (
        <AceEditor
          className={expandedEditorTw}
          mode={mode}
          minLines={10}
          maxLines={30}
          width="100%"
          value={value}
          showPrintMargin={false}
          onChange={onAceEditorChange}
          showGutter={false}
          focus
          setOptions={{ useWorker: false }}
        />
      )}
    </div>
  );
};
