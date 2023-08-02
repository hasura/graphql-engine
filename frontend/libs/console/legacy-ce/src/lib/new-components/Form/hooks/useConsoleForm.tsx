import { DevTool } from '@hookform/devtools';
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

export type UseConsoleFormReturn = {
  methods: ReturnType<typeof useReactHookForm>;
  Form: <TFieldValues extends FieldValues>(
    props: FormProps<TFieldValues>
  ) => JSX.Element;
};

// available as a standlone if needed for advanced usage
const ConsoleFormWrapper = <
  TFieldValues extends FieldValues,
  TSchema extends zodInfer<Schema>,
  // eslint-disable-next-line
  TContext extends object = object
>(
  props: FormWrapperProps<TFieldValues, TSchema, TContext>
) => {
  const { id, className, onSubmit, children, debug, ...methods } = props;
  return (
    <FormProvider {...methods}>
      <form
        id={id}
        className={className}
        onSubmit={methods.handleSubmit(onSubmit)}
        data-non-regression="new-form-pattern"
      >
        {debug && (
          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          <DevTool control={methods.control as any} />
        )}
        {children}
      </form>
    </FormProvider>
  );
};

export const useConsoleForm = <FormSchema extends Schema>(
  hookProps: UseConsoleFormProps<zodInfer<FormSchema>, FormSchema>
): UseConsoleFormReturn => {
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
