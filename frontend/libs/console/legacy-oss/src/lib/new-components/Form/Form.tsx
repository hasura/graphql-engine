import { zodResolver } from '@hookform/resolvers/zod';
import * as React from 'react';
import {
  useForm,
  UseFormReturn,
  SubmitHandler,
  UseFormProps,
  FormProvider,
  Path,
} from 'react-hook-form';
import { ZodType, ZodTypeDef, infer as zodInfer } from 'zod';

type FormProps<TFormValues, Schema> = {
  /**
   * Classes to apply to the wrapped <form> element
   */
  className?: string;
  /**
   * On submit handler
   */
  onSubmit: SubmitHandler<TFormValues>;
  /**
   * The component children
   * @param methods
   */
  children: (methods: UseFormReturn<TFormValues>) => React.ReactNode;
  /**
   * The form options
   */
  options?: UseFormProps<TFormValues>;
  /**
   * The form ID
   */
  id?: string;
  /**
   * The form validation schema
   */
  schema: Schema;
};

export const Form = React.forwardRef(
  <
    TFormValues extends Record<string, unknown> = Record<string, unknown>,
    Schema extends ZodType<TFormValues, ZodTypeDef, TFormValues> = ZodType<
      TFormValues,
      ZodTypeDef,
      TFormValues
    >
  >(
    {
      onSubmit,
      children,
      className,
      options,
      id,
      schema,
    }: FormProps<zodInfer<Schema>, Schema>,
    ref: React.Ref<unknown>
  ) => {
    const methods = useForm<zodInfer<Schema>>({
      ...options,
      resolver: schema && zodResolver(schema),
    });
    React.useImperativeHandle(ref, () => ({
      trigger: async () => {
        return methods.trigger();
      },
      setFocus: (name: string) => {
        return methods.setFocus(name as Path<unknown>);
      },
    }));
    return (
      <FormProvider {...methods}>
        <form
          id={id}
          className={`space-y-md bg-legacybg p-4 ${className || ''}`}
          onSubmit={methods.handleSubmit(onSubmit)}
        >
          {children(methods)}
        </form>
      </FormProvider>
    );
  }
);
