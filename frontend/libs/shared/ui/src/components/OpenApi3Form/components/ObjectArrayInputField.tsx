import { OpenApiReference, OpenApiSchema } from '@hasura/dc-api-types';
import { useState } from 'react';
import get from 'lodash/get';
import { FaEdit, FaTrash } from 'react-icons/fa';
import { FieldError, useFormContext } from 'react-hook-form';
import {
  getInputAttributes,
  getReferenceObject,
  isReferenceObject,
} from '../utils';
import { RenderProperty } from './RenderProperty';
import { isObjectInputField } from './ObjectInputField';
import { FieldWrapper } from '../../Form';
import { CardedTable } from '../../table/CardedTable';
import { Button } from '../../Button';
import { Text } from '../../typography';
import { JsonCodeBlock } from '../../codeblock';
import { Em } from '@radix-ui/themes';

export const isObjectArrayInputField = (
  configSchema: OpenApiSchema,
  otherSchemas: Record<string, OpenApiSchema>,
): configSchema is OpenApiSchema & {
  properties: Record<string, OpenApiSchema | OpenApiReference>;
  type: 'object';
  items: OpenApiSchema | OpenApiReference;
} => {
  const { type, items } = configSchema;

  /**
   * check if the type is object and it has properties!!
   */
  if (type === 'array' && items) {
    const itemSchema = isReferenceObject(items)
      ? getReferenceObject(items.$ref, otherSchemas)
      : items;
    if (itemSchema.type === 'object') return true;
  }

  return false;
};

export const ObjectArrayInputField = ({
  name,
  configSchema,
  otherSchemas,
}: {
  name: string;
  configSchema: OpenApiSchema & {
    properties: Record<string, OpenApiSchema | OpenApiReference>;
    type: 'object';
    items: OpenApiSchema | OpenApiReference;
  };
  otherSchemas: Record<string, OpenApiSchema>;
}) => {
  const { label, tooltip } = getInputAttributes(name, configSchema);

  const { items } = configSchema;

  const itemSchema = isReferenceObject(items)
    ? getReferenceObject(items.$ref, otherSchemas)
    : items;

  const {
    setValue,
    watch,
    formState: { errors },
  } = useFormContext();

  const formValues: Record<string, any>[] = watch(name);
  const maybeError = get(errors, name);
  const [activeRecord, setActiveRecord] = useState<number | undefined>();

  if (!isObjectInputField(itemSchema)) return null;

  return (
    <FieldWrapper
      id={name}
      error={maybeError as FieldError}
      label={label}
      size="full"
      tooltip={tooltip}
    >
      {formValues?.length ? (
        <CardedTable
          columns={['No.', 'Value', 'Actions']}
          data={formValues.map((value, index) => {
            return [
              index + 1,
              <JsonCodeBlock key={`value-${index}`} value={value} size="1" />,
              <div key={`actions-${index}`} className="gap-4 flex">
                <Button
                  onClick={() => setActiveRecord(index)}
                  disabled={activeRecord === index}
                  leftIcon={FaEdit}
                >
                  Edit
                </Button>
                <Button
                  onClick={() => {
                    setValue(
                      name,
                      formValues.filter((_x, i) => i !== index),
                    );
                    setActiveRecord(undefined);
                  }}
                  mode="destructive"
                  leftIcon={FaTrash}
                >
                  Remove
                </Button>
              </div>,
            ];
          })}
        />
      ) : (
        <Text>
          <Em>No {name} entries found.</Em>
        </Text>
      )}

      {activeRecord !== undefined ? (
        <div className="bg-white p-6 border border-gray-300 rounded space-y-4 mb-6 max-w-xl ">
          {Object.entries(itemSchema.properties).map(
            ([propertyName, property]) => {
              return (
                <RenderProperty
                  name={`${name}.${activeRecord}.${propertyName}`}
                  configSchema={property}
                  otherSchemas={otherSchemas}
                  key={`${name}.${activeRecord}.${propertyName}`}
                />
              );
            },
          )}
          <div>
            <Button onClick={() => setActiveRecord(undefined)}>Close</Button>
          </div>
        </div>
      ) : null}
      <div>
        <Button
          onClick={() => {
            setValue(name, formValues ? [...formValues, {}] : [{}]);
            setActiveRecord(formValues?.length ?? 0);
          }}
        >
          Add New Entry
        </Button>
      </div>
    </FieldWrapper>
  );
};
