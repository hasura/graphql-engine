import React from 'react';
import clsx from 'clsx';
import get from 'lodash.get';
import AceEditor, { IAceOptions, IEditorProps } from 'react-ace';
import 'ace-builds/src-noconflict/theme-github';
import 'ace-builds/src-noconflict/theme-eclipse';
import 'ace-builds/src-noconflict/ext-language_tools';
import { FieldError, useFormContext, Controller } from 'react-hook-form';
import { FieldWrapper, FieldWrapperPassThroughProps } from './FieldWrapper';

export type CodeEditorFieldProps = FieldWrapperPassThroughProps & {
  /**
   * The code editor field name
   */
  name: string;
  /**
   * The code editor props
   */
  editorProps?: IEditorProps;
  /**
   * The code editor options
   */
  editorOptions?: IAceOptions;
  /**
   * The theme of code editor
   */
  theme?: 'github' | 'eclipse';
  /**
   * The mode of code editor
   */
  mode?: string;
  /**
   * Flag to indicate if the field is disabled
   */
  disabled?: boolean;
};

export const CodeEditorField: React.FC<CodeEditorFieldProps> = ({
  name,
  editorProps,
  editorOptions = { minLines: 5, maxLines: 8, showGutter: false },
  theme = 'github',
  mode = 'json',
  disabled,
  dataTest,
  ...wrapperProps
}: CodeEditorFieldProps) => {
  const {
    control,
    formState: { errors },
  } = useFormContext();
  const maybeError = get(errors, name) as FieldError | undefined;

  return (
    <FieldWrapper id={name} {...wrapperProps} error={maybeError}>
      <div className={clsx(disabled ? 'cursor-not-allowed' : '')}>
        <Controller
          name={name}
          control={control}
          render={({
            field: { value, name: controllerName, ref, onChange, onBlur },
          }) => (
            <AceEditor
              name={controllerName}
              ref={ref}
              value={value}
              theme={theme}
              mode={mode}
              onChange={onChange}
              onBlur={onBlur}
              editorProps={editorProps}
              setOptions={editorOptions}
              data-test={dataTest}
              className={clsx(
                'block relative inset-0 !w-inherit max-w-xl h-code input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-0 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400 placeholder-gray-500',
                maybeError
                  ? 'border-red-600 hover:border-red-700'
                  : 'border-gray-300',
                disabled
                  ? 'bg-gray-100 border-gray-100 pointer-events-none'
                  : 'hover:border-gray-400'
              )}
              data-testid={name}
            />
          )}
        />
      </div>
    </FieldWrapper>
  );
};
