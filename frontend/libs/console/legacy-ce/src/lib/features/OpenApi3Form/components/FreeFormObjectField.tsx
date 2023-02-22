import { CodeEditorField } from '../../../new-components/Form';
import { OpenApiSchema } from '@hasura/dc-api-types';
import React from 'react';
import { getInputAttributes } from '../utils';

export const isFreeFormObjectField = (configSchema: OpenApiSchema): boolean => {
  const { type, properties } = configSchema;

  /**
   * check if the type is object and it has properties!!
   */
  if (type === 'object' && !properties) return true;

  return false;
};

export const FreeFormObjectField = ({
  name,
  configSchema,
}: {
  name: string;
  configSchema: OpenApiSchema;
}) => {
  const { label, tooltip } = getInputAttributes(name, configSchema);

  return (
    <div className="max-w-xl flex justify-between my-4">
      <CodeEditorField
        name={name}
        label={label}
        editorOptions={{
          mode: 'code',
          minLines: 5,
          maxLines: 8,
          showGutter: true,
        }}
        tooltip={tooltip}
      />
    </div>
  );
};
