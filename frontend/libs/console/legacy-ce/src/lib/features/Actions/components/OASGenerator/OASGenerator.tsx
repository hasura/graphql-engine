import { SimpleForm } from '@/new-components/Form';
import React from 'react';
import { z } from 'zod';
import { GeneratedAction } from '../OASGeneratorModal';
import { OasGeneratorForm } from './OASGeneratorForm';

export interface OasGeneratorProps {
  onGenerate: (values: GeneratedAction) => void;
}

export const formSchema = z.object({
  oas: z.string(),
  operation: z.string(),
  url: z.string().url({ message: 'Invalid URL' }),
  search: z.string(),
});

export const OasGenerator = (props: OasGeneratorProps) => {
  const [values, setValues] = React.useState<GeneratedAction>();

  return (
    <SimpleForm
      schema={formSchema}
      onSubmit={() => {
        if (values) {
          props.onGenerate(values);
        }
      }}
    >
      <OasGeneratorForm setValues={setValues} />
    </SimpleForm>
  );
};
