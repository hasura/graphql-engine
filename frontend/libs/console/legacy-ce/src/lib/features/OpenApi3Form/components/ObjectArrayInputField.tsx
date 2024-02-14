import { Button } from '../../../new-components/Button';
import { CardedTable } from '../../../new-components/CardedTable';
import { FieldWrapper } from '../../../new-components/Form';
import { OpenApiReference, OpenApiSchema } from '@hasura/dc-api-types';
import React, { useState } from 'react';
import get from 'lodash/get';
import { FaEdit, FaTrash } from 'react-icons/fa';
import { useFormContext } from 'react-hook-form';
import ReactJson from 'react-json-view';
import {
  getInputAttributes,
  getReferenceObject,
  isReferenceObject,
} from '../utils';
import { RenderProperty } from './RenderProperty';
import { isObjectInputField } from './ObjectInputField';

export const isObjectArrayInputField = (
  configSchema: OpenApiSchema,
  otherSchemas: Record<string, OpenApiSchema>
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
      error={maybeError}
      label={label}
      size="full"
      tooltip={tooltip}
    >
      {formValues?.length ? (
        <CardedTable.Table>
          <CardedTable.TableHead>
            <CardedTable.TableHeadRow>
              <CardedTable.TableHeadCell>No.</CardedTable.TableHeadCell>
              <CardedTable.TableHeadCell>Value</CardedTable.TableHeadCell>
              <CardedTable.TableHeadCell>Actions</CardedTable.TableHeadCell>
            </CardedTable.TableHeadRow>
          </CardedTable.TableHead>

          <CardedTable.TableBody>
            {formValues.map((value, index) => {
              return (
                <CardedTable.TableBodyRow key={`${index}`}>
                  <CardedTable.TableBodyCell>
                    {index + 1}
                  </CardedTable.TableBodyCell>
                  <CardedTable.TableBodyCell>
                    <ReactJson
                      src={JSON.parse(JSON.stringify(value))}
                      name={false}
                      collapsed
                    />
                  </CardedTable.TableBodyCell>
                  <CardedTable.TableBodyCell>
                    <div className="gap-4 flex">
                      <Button
                        onClick={() => setActiveRecord(index)}
                        disabled={activeRecord === index}
                        icon={<FaEdit />}
                      >
                        Edit
                      </Button>
                      <Button
                        onClick={() => {
                          setValue(
                            name,
                            formValues.filter((_x, i) => i !== index)
                          );
                          setActiveRecord(undefined);
                        }}
                        mode="destructive"
                        icon={<FaTrash />}
                      >
                        Remove
                      </Button>
                    </div>
                  </CardedTable.TableBodyCell>
                </CardedTable.TableBodyRow>
              );
            })}
          </CardedTable.TableBody>
        </CardedTable.Table>
      ) : (
        <div className="italic py-4">No {name} entries found.</div>
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
            }
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
