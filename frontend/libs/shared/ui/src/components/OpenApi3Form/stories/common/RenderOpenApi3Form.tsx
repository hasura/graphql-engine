import { OpenApiSchema } from '@hasura/dc-api-types';
import { useState } from 'react';
import { z } from 'zod';
import { OpenApi3Form, useZodSchema } from '../../components/OpenApi3Form';
import { useConsoleForm } from '../../../Form';
import { Button } from '../../../Button';
import { JsonCodeBlock } from '../../../codeblock';

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
    {},
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
      onSubmit={(values) => {
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
            <JsonCodeBlock value={submittedValues} />
          )}
        </div>
      </>
    </Form>
  );
};
