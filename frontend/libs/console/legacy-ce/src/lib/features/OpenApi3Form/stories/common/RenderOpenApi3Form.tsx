import { useConsoleForm } from '../../../../new-components/Form';
import { Button } from '../../../../new-components/Button';
import { OpenApiSchema } from '@hasura/dc-api-types';
import React, { useState } from 'react';
import ReactJson from 'react-json-view';
import { z } from 'zod';
import { OpenApi3Form, useZodSchema } from '../../components/OpenApi3Form';

export const RenderOpenApi3Form = ({
  getSchema,
  defaultValues,
  name,
  rawOutput,
}: {
  getSchema: () => [OpenApiSchema, Record<string, OpenApiSchema>];
  defaultValues: Record<string, any>;
  name: string;
  rawOutput?: boolean;
}) => {
  const [submittedValues, setSubmittedValues] = useState<Record<string, any>>(
    {}
  );

  const [configSchema, otherSchemas] = getSchema();
  const { data: schema, isLoading } = useZodSchema({
    configSchema,
    otherSchemas,
  });
  const { Form } = useConsoleForm({
    schema: z.object(schema ? { [name]: schema } : {}),
    options: {
      defaultValues,
    },
  });

  if (!schema || isLoading) return <>Loading...</>;

  return (
    <Form
      onSubmit={values => {
        setSubmittedValues(values as any);
      }}
    >
      <>
        <OpenApi3Form
          schemaObject={configSchema}
          references={otherSchemas}
          name={name}
        />
        <Button type="submit" data-testid="submit-form-btn">
          Submit
        </Button>
        <div>Submitted Values:</div>
        <div data-testid="output">
          {rawOutput ? (
            JSON.stringify(submittedValues)
          ) : (
            <ReactJson src={submittedValues} name={false} />
          )}
        </div>
      </>
    </Form>
  );
};
