import { useState } from 'react';
import { FaCompressAlt, FaExpandAlt } from 'react-icons/fa';
import { AceEditor } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

import { TextInput, TextInputProps } from './TextInput';

const baseInputTw =
  'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400 placeholder:text-slate-400';

export type ExpandableTextInputProps = {
  mode?: 'normal' | 'json' | 'text' | 'markdown' | 'html';
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

  const toggleModeIcon = isBigEditorActive ? (
    <FaCompressAlt
      className="fill-slate-300 hover:fill-slate-400"
      onClick={() => setBigEditorActive((prev) => !prev)}
      title="Collapse"
    />
  ) : (
    <FaExpandAlt
      className="fill-slate-300 hover:fill-slate-400"
      onClick={() => setBigEditorActive((prev) => !prev)}
      title="Expand"
    />
  );

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
      <Flex className="cursor-pointer absolute right-2 top-[10px] z-10 h-[14px]">
        {toggleModeIcon}
      </Flex>
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
