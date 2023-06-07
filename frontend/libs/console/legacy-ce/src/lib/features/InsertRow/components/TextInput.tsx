import { InputRef } from '../../../new-components/Form/Input';
import clsx from 'clsx';
import { ChangeHandler } from 'react-hook-form';

export type InputCustomEvent = { target: { value: string } };

export type CustomEventHandler = (event: InputCustomEvent) => void;

export type TextInputProps = Omit<
  React.InputHTMLAttributes<HTMLInputElement>,
  'onChange' | 'onInput' | 'onBlur'
> & {
  name: string;
  disabled: boolean;
  placeholder: string;
  inputRef?: InputRef;
  onChange?: React.ChangeEventHandler<HTMLInputElement> | CustomEventHandler;
  onInput?: React.FormEventHandler<HTMLInputElement> | CustomEventHandler;
  onBlur?: React.FocusEventHandler<HTMLInputElement> | ChangeHandler;
  className?: string;
};

export const baseInputTw =
  'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400 placeholder:text-slate-400';

export const TextInput: React.VFC<TextInputProps> = ({
  name,
  disabled,
  placeholder,
  inputRef,
  onChange,
  onInput,
  onBlur,
  className,
  ...rest
}) => {
  const disabledInputTw = '!bg-slate-100 !border-slate-200 cursor-not-allowed';
  const inputTw = clsx(baseInputTw, disabled && disabledInputTw);

  return (
    <input
      {...rest}
      name={name}
      onChange={onChange}
      onInput={onInput as React.FormEventHandler<HTMLInputElement>}
      onBlur={onBlur}
      ref={inputRef}
      type="text"
      className={clsx(inputTw, className || '')}
      disabled={disabled}
      placeholder={placeholder}
    />
  );
};
