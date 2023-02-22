import React from 'react';
import { GeneratedAction } from './types';
import { SimpleForm } from '../../../../new-components/Form';
import { OasGeneratorForm } from './OASGeneratorForm';
import { formSchema } from './OASGeneratorPage';

export interface OASGeneratorProps {
  onGenerate: (action: GeneratedAction) => void;
  onDelete: (actionName: string) => void;
  disabled: boolean;
}

export const OASGenerator = (props: OASGeneratorProps) => {
  const { onGenerate, onDelete, disabled } = props;
  return (
    <SimpleForm onSubmit={() => {}} schema={formSchema}>
      <OasGeneratorForm
        onGenerate={onGenerate}
        onDelete={onDelete}
        disabled={disabled}
      />
    </SimpleForm>
  );
};
