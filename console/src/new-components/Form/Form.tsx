import { zodResolver } from '@hookform/resolvers/zod';
import * as React from 'react';
import {
  useForm,
  UseFormReturn,
  SubmitHandler,
  UseFormProps,
  FormProvider,
} from 'react-hook-form';
import { ZodType, ZodTypeDef, infer as zodInfer } from 'zod';

type FormProps<TFormValues, Schema> = {
  className?: string;
  onSubmit: SubmitHandler<TFormValues>;
  children: (methods: UseFormReturn<TFormValues>) => React.ReactNode;
  options?: UseFormProps<TFormValues>;
  id?: string;
  schema: Schema;
};

export const Form = <
  TFormValues extends Record<string, unknown> = Record<string, unknown>,
  Schema extends ZodType<TFormValues, ZodTypeDef, TFormValues> = ZodType<
    TFormValues,
    ZodTypeDef,
    TFormValues
  >
>({
  onSubmit,
  children,
  className,
  options,
  id,
  schema,
}: FormProps<zodInfer<Schema>, Schema>) => {
  const methods = useForm<zodInfer<Schema>>({
    ...options,
    resolver: schema && zodResolver(schema),
  });
  return (
    <FormProvider {...methods}>
      <form
        className={`space-y-md ${className}`}
        onSubmit={methods.handleSubmit(onSubmit)}
        id={id}
      >
        {children(methods)}
      </form>
    </FormProvider>
  );
};
