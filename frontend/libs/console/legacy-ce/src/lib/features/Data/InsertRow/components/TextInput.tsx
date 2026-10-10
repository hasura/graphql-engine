import { Input, InputProps } from '@hasura/shared/ui';

export type InputCustomEvent = { target?: { value?: string } };

export type CustomEventHandler = (event: InputCustomEvent) => void;

export type TextInputProps = Omit<
  InputProps,
  'onChange' | 'onInput' | 'onBlur' | 'ref'
> & {
  name?: string;
  ref?: React.ForwardedRef<unknown>;
  onChange?: React.ChangeEventHandler<HTMLInputElement>;
  onInput?: React.FormEventHandler<HTMLInputElement>;
  onBlur?: React.FocusEventHandler<HTMLInputElement>;
};

export const TextInput: React.FC<TextInputProps> = ({
  name,
  ref,
  onChange,
  onInput,
  onBlur,
  className,
  defaultValue,
  ...rest
}) => {
  return (
    <Input
      {...rest}
      name={name}
      onChange={onChange}
      onInput={onInput as React.FormEventHandler<HTMLInputElement>}
      onBlur={onBlur}
      ref={ref}
      type="text"
      defaultValue={defaultValue}
    />
  );
};
