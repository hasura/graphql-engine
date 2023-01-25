import { zodResolver } from '@hookform/resolvers/zod';
import * as React from 'react';
import {
  FieldValues,
  FormProvider,
  useForm as useReactHookForm,
} from 'react-hook-form';
import { infer as zodInfer } from 'zod';
import {
  FormProps,
  FormWrapperProps,
  Schema,
  UseConsoleFormProps,
} from './form.types';

// available as a standlone if needed for advanced usage
const ConsoleFormWrapper = <
  TFieldValues extends FieldValues,
  TSchema extends zodInfer<Schema>,
  // eslint-disable-next-line
  TContext extends object = object
>(
  props: FormWrapperProps<TFieldValues, TSchema, TContext>
) => {
  const { id, className, onSubmit, children, ...methods } = props;
  return (
    <FormProvider {...methods}>
      <form
        id={id}
        className={className}
        onSubmit={methods.handleSubmit(onSubmit)}
        data-non-regression="new-form-pattern"
      >
        {children}
      </form>
    </FormProvider>
  );
};

export const useConsoleForm = <FormSchema extends Schema>(
  hookProps: UseConsoleFormProps<zodInfer<FormSchema>, FormSchema>
) => {
  const { options = {}, schema } = hookProps;

  const methods = useReactHookForm<zodInfer<FormSchema>>({
    ...options,
    resolver: schema && zodResolver(schema),
  });

  const BoundWrapper = React.useMemo(
    () =>
      <TFieldValues extends zodInfer<typeof schema>>(
        props: FormProps<TFieldValues>
      ) =>
        (
          <ConsoleFormWrapper {...methods} {...props}>
            {props.children}
          </ConsoleFormWrapper>
        ),
    [methods]
  );

  return {
    methods,
    Form: BoundWrapper,
  };
};
