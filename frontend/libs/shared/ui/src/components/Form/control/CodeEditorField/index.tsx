import React from 'react';
import { useFormContext, Controller } from 'react-hook-form';
import { FieldWrapper, FieldWrapperPassThroughProps } from '../FieldWrapper';
import { AceEditor, AceEditorProps } from '../../base';
import { IAceOptions } from 'react-ace';

export type CodeEditorFieldProps = FieldWrapperPassThroughProps & {
  /**
   * The code editor field name
   */
  name: string;
  /**
   * The placeholder text to display when the field is not valued
   */
  placeholder?: string;
  /**
   * The code editor props
   */
  editorProps?: Omit<AceEditorProps, 'disabled' | 'placeholder' | 'setOptions'>;
  /**
   * The code editor options
   */
  editorOptions?: IAceOptions;

  /**
   * Flag to indicate if the field is disabled
   */
  disabled?: boolean;
};

export const CodeEditorField: React.FC<CodeEditorFieldProps> = ({
  name,
  editorProps,
  disabled,
  dataTest,
  placeholder,
  editorOptions,
  ...wrapperProps
}: CodeEditorFieldProps) => {
  const { control } = useFormContext();

  return (
    <Controller
      name={name}
      control={control}
      render={({ field: { value, ...controlProps }, fieldState }) => {
        return (
          <FieldWrapper id={name} {...wrapperProps} error={fieldState.error}>
            <AceEditor
              value={typeof value === 'string' ? value : JSON.stringify(value)}
              data-test={dataTest}
              data-testid={name}
              invalid={fieldState.invalid}
              placeholder={placeholder}
              setOptions={editorOptions}
              disabled={disabled}
              {...controlProps}
              {...editorProps}
            />
          </FieldWrapper>
        );
      }}
    />
  );
};
