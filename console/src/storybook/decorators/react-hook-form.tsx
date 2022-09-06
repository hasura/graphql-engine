import React from 'react';
import { FormProvider, useForm } from 'react-hook-form';

const Form: React.FC = ({ children }) => {
  const methods = useForm();

  return <FormProvider {...methods}>{children}</FormProvider>;
};

export const FormDecorator = () => (Story: any) =>
  (
    <Form>
      <Story />
    </Form>
  );
