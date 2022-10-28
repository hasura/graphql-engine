import React from 'react';
import { useFormContext } from 'react-hook-form';

import { GraphQLSchema } from 'graphql';

import { CustomField } from './Fields';
import { FieldArray } from './FieldArray';
import { Builder } from './Builder';
import { JsonItem } from './Elements';
import { findColumnOperator, getColumnOperators } from '../utils';

interface Props {
  columnKey: string;
  tableName: string;
  dropDownState: any;
  columnOperators: ReturnType<typeof getColumnOperators>;
  handleColumnChange: (i: any) => void;
  /**
   * The builder is a recursive structure
   * the nesting describes the level of the structure
   * so react hook form can correctly register the fields
   * e.g. ['filter', 'Title', '_eq'] would be registered as 'filter.Title._eq'
   */
  nesting: string[];
  schema: GraphQLSchema;
}

export const RenderFormElement = (props: Props) => {
  const {
    columnKey,
    tableName,
    dropDownState,
    columnOperators,
    handleColumnChange,
    nesting,
    schema,
  } = props;

  const { register, setValue, watch } = useFormContext();

  const selectedOperator = React.useMemo(
    () => findColumnOperator({ columnKey, columnOperators }),
    [columnOperators, columnKey]
  );
  const val = watch(columnKey);
  const selectedOperatorType = React.useMemo(() => {
    if (typeof val === 'string' && val.startsWith('X-Hasura-')) {
      return 'text';
    }

    return selectedOperator?.type?.type === 'String' ? 'text' : 'number';
  }, [selectedOperator, val]);

  const setValueAs = (value: 'text' | 'number') => {
    const isNumber = !Number.isNaN(parseInt(value, 10));
    if (isNumber) {
      return parseInt(value, 10);
    }

    return value;
  };

  switch (dropDownState?.type) {
    case 'column':
      if (!selectedOperator) {
        return null;
      }
      return (
        <div className="flex flex-col">
          <div className="flex items-center px-6 border-dashed border-l border-gray-200 py-4">
            {!!columnOperators.length && (
              <CustomField.Select
                title="Column Operator"
                value={dropDownState.columnOperator}
                onChange={handleColumnChange}
              >
                {columnOperators.map(({ name }) => {
                  return (
                    <option key={name} value={name}>
                      {name}
                    </option>
                  );
                })}
              </CustomField.Select>
            )}

            <JsonItem text=":" className="mr-4" />
            <div className="flex gap-2">
              <CustomField.Input
                key={`${columnKey}-${selectedOperatorType}`}
                title="Column value"
                type={selectedOperatorType}
                {...register(columnKey, {
                  setValueAs,
                })}
              />
              <button
                type="button"
                className="text-blue-800 font-bold text-sm"
                onClick={() => setValue(columnKey, 'X-Hasura-User-Id')}
              >
                [X-Hasura-User-Id]
              </button>
            </div>
          </div>
          <JsonItem text="}" />
        </div>
      );
    // when rendering a relationship the name of the table needs to be updated
    // to look for values from the connected table
    case 'relationship':
      return (
        <div className="border-l-dashed border-gray-200">
          <div className="py-4">
            <Builder
              key={columnKey}
              tableName={dropDownState.typeName}
              nesting={[...nesting, dropDownState.name]}
              schema={schema}
            />
          </div>
          <JsonItem text="}" />
        </div>
      );

    case 'boolOperator':
      return (
        <>
          {dropDownState.name === '_not' && (
            <div className="border-l-dashed border-gray-200">
              <div className="py-4">
                <Builder
                  key={columnKey}
                  tableName={tableName}
                  nesting={[...nesting, dropDownState.name]}
                  schema={schema}
                />
              </div>
              <JsonItem text="}" />
            </div>
          )}

          {(dropDownState.name === '_and' || dropDownState.name === '_or') && (
            <FieldArray
              key={columnKey}
              tableName={tableName}
              nesting={[...nesting, dropDownState.name]}
              schema={schema}
            />
          )}
        </>
      );
    default:
      return null;
  }
};
