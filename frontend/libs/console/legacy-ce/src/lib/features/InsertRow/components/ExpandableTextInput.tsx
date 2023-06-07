import { useState } from 'react';
import { FaCompressAlt, FaExpandAlt } from 'react-icons/fa';
import AceEditor from 'react-ace';
import { baseInputTw, TextInput, TextInputProps } from './TextInput';

import 'brace/mode/html';
import 'brace/mode/markdown';
import 'brace/theme/github';
import 'brace/theme/chrome';

export type ExpandableTextInputProps = {
  mode?: 'normal' | 'json' | 'text' | 'markdown' | 'html';
  theme?: 'chrome' | 'github';
} & TextInputProps;

export const ExpandableTextInput: React.VFC<ExpandableTextInputProps> = ({
  mode = 'normal',
  theme = 'chrome',
  onChange,
  onInput,
  ...restProps
}) => {
  const [isBigEditorActive, setBigEditorActive] = useState(false);

  const toggleModeIcon = isBigEditorActive ? (
    <FaCompressAlt
      className="fill-slate-300 hover:fill-slate-400"
      onClick={() => setBigEditorActive(prev => !prev)}
      title="Collapse"
    />
  ) : (
    <FaExpandAlt
      className="fill-slate-300 hover:fill-slate-400"
      onClick={() => setBigEditorActive(prev => !prev)}
      title="Expand"
    />
  );

  const [value, setValue] = useState('');
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
      <div className="cursor-pointer absolute flex right-2 top-[10px] z-10 h-[14px]">
        {toggleModeIcon}
      </div>
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
          className={baseInputTw}
          mode={mode}
          theme={theme}
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
