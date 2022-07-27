import React from 'react';
import clsx from 'clsx';
import get from 'lodash.get';
import AceEditor, { IAceOptions, IEditorProps } from 'react-ace';
import 'ace-builds/src-noconflict/theme-github';
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
   * The code editor field size
   */
  size?: 'full' | 'medium';
  /**
   * Flag to indicate if the field is disabled
   */
  disabled?: boolean;
};

export const CodeEditorField: React.FC<CodeEditorFieldProps> = ({
  name,
  editorProps,
  editorOptions = { minLines: 5, maxLines: 8, showGutter: false },
  size = 'full',
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
    <FieldWrapper
      id={name}
      {...wrapperProps}
      className={size === 'medium' ? 'w-1/2 max-w-lg' : 'w-full max-w-xl'}
      error={maybeError}
    >
      <div
        className={clsx(
          'relative',
          size === 'medium' ? 'w-1/2 max-w-lg' : 'w-full max-w-xl',
          disabled ? 'cursor-not-allowed' : ''
        )}
      >
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
              theme="github"
              onChange={onChange}
              onBlur={onBlur}
              editorProps={editorProps}
              setOptions={editorOptions}
              data-test={dataTest}
              className={clsx(
                'block w-full max-w-xl max-w-full h-code input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400 placeholder-gray-500',
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
