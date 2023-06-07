import React from 'react';
import {
  FieldValues,
  FormProviderProps,
  SubmitHandler,
  UseFormProps,
} from 'react-hook-form';
import { infer as ZodInfer, ZodType, ZodTypeDef } from 'zod';

// hook props:
export type UseConsoleFormProps<
  TFielValues extends FieldValues,
  TSchema extends ZodType<any, any, any>
> = {
  /**
   * The form options
   */
  options?: UseFormProps<TFielValues>;
  /**
   * The form validation schema
   */
  schema: TSchema;
};

type TFormValues = Record<string, unknown>;

export type Schema = ZodType<TFormValues, ZodTypeDef, TFormValues>;

// form component props:
export type FormProps<TForm extends ZodInfer<Schema>> = {
  /**
   * Classes to apply to the wrapped <form> element
   */
  className?: string;
  /**
   * On submit handler
   */
  onSubmit: SubmitHandler<TForm>;
  /**
   * The component children
   */
  children: React.ReactNode;
  /**
   * The form ID
   */
  id?: string;
  /**
   * Enables a debug pane that outputs form values and errors
   */
  debug?: boolean;
};

// form wrapper props (combo of form component and FormProvider)
export interface FormWrapperProps<
  TFieldValues extends FieldValues,
  TSchema extends ZodInfer<Schema>,
  // eslint-disable-next-line
  TContext extends object = object
> extends FormProviderProps<TFieldValues, TContext>,
    FormProps<TSchema> {}
