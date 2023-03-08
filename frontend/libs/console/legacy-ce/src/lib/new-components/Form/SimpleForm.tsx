import { v4 as uuid } from 'uuid';
import { infer as zodInfer } from 'zod';
import { useConsoleForm } from './hooks/useConsoleForm';
import { FormProps, UseConsoleFormProps, Schema } from './hooks/form.types';

export const SimpleForm = <FormSchema extends Schema>(
  props: FormProps<zodInfer<FormSchema>> &
    UseConsoleFormProps<zodInfer<FormSchema>, FormSchema>
) => {
  const { Form } = useConsoleForm<FormSchema>(props);
  return <Form {...props} />;
};

SimpleForm.defaultProps = {
  className: '',
  id: uuid(),
  options: {},
};
