import React from 'react';
import { useFieldArray, useFormContext } from 'react-hook-form';

import { GraphQLSchema } from 'graphql';

import { Builder } from './Builder';
import { JsonItem } from './Elements';

interface FieldArrayElementProps {
  index: number;
  arrayKey: string;
  tableName: string;
  nesting: string[];
  field: Field;
  fields: Field[];
  append: ReturnType<typeof useFieldArray>['append'];
  schema: GraphQLSchema;
}

type Field = Record<'id', string>;

export const FieldArrayElement = (props: FieldArrayElementProps) => {
  const { index, arrayKey, tableName, field, nesting, fields, append, schema } =
    props;
  const { watch } = useFormContext();

  // from this we can determine if the dropdown has been selected
  // if it has and the element is the final field
  // another element needs to be appended
  const currentField = watch(`operators.${arrayKey}.${index}`);
  const isFinalField = fields.length - 1 === index;

  if (currentField && isFinalField) {
    append({});
  }

  if (isFinalField) {
    return (
      <>
        <div className="px-6">
          <JsonItem text="{" />
          <Builder
            key={field.id}
            tableName={tableName}
            nesting={[...nesting, index.toString()]}
            schema={schema}
          />
          <JsonItem text="}" />
        </div>
        <JsonItem text="]" />
      </>
    );
  }

  return (
    <div className="px-6">
      <JsonItem text="{" />
      <Builder
        key={field.id}
        tableName={tableName}
        nesting={[...nesting, index.toString()]}
        schema={schema}
      />
      <JsonItem text="}," />
    </div>
  );
};

interface Props {
  tableName: string;
  nesting: string[];
  schema: GraphQLSchema;
}

export const FieldArray = (props: Props) => {
  const { tableName, nesting, schema } = props;
  const arrayKey = nesting.join('.');

  const { fields, append } = useFieldArray({
    name: arrayKey,
  });

  // automatically append a new field when the array is empty
  // necessary to render an element
  if (fields.length === 0) {
    append({});
  }

  return (
    <div
      key={arrayKey}
      className="flex flex-col w-full border-dashed border-l border-gray-200"
    >
      <div className="flex flex-col py-2">
        {fields.map((field, index) => (
          <FieldArrayElement
            key={field.id}
            index={index}
            arrayKey={arrayKey}
            tableName={tableName}
            field={field}
            fields={fields}
            nesting={nesting}
            append={append}
            schema={schema}
          />
        ))}
      </div>
    </div>
  );
};
