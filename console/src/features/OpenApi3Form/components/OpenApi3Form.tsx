import { useQuery } from 'react-query';
import React from 'react';
import { OpenApiSchema } from '@hasura/dc-api-types';
import { transformSchemaToZodObject } from '../utils';
import { RenderProperty } from './RenderProperty';

export const useZodSchema = ({
  configSchema,
  otherSchemas,
}: {
  configSchema: OpenApiSchema;
  otherSchemas: Record<string, OpenApiSchema>;
}) => {
  return useQuery({
    queryFn: async () => {
      const zodSchema = transformSchemaToZodObject(configSchema, otherSchemas);
      return zodSchema;
    },
  });
};

interface OpenApi3FormProps {
  name: string;
  schemaObject: OpenApiSchema;
  references: Record<string, OpenApiSchema>;
}

export const OpenApi3Form = (props: OpenApi3FormProps) => {
  return (
    <RenderProperty
      name={props.name}
      configSchema={props.schemaObject}
      otherSchemas={props.references}
    />
  );
};
