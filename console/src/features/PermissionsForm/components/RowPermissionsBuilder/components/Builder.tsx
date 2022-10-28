import React from 'react';
import { useFormContext } from 'react-hook-form';
import { GraphQLSchema } from 'graphql';

import { RenderFormElement } from './RenderFormElement';
import { CustomField } from './Fields';
import { JsonItem } from './Elements';

import { getColumnOperators } from '../utils';

import { useData } from '../hooks';

const createKey = (inputs: string[]) => inputs.filter(Boolean).join('.');

interface Args {
  value: string;
  data: ReturnType<typeof useData>['data'];
}

/**
 * return value to be set for dropdown state
 */
const getNewValues = ({ value, data }: Args) => {
  const allItemsArray = [
    ...data.boolOperators,
    ...data.columns,
    ...data.relationships,
  ];
  const selectedItem = allItemsArray.find(item => item.name === value);

  switch (selectedItem?.kind) {
    case 'boolOperator':
      return {
        name: selectedItem.name,
        typeName: selectedItem.name,
        type: 'boolOperator',
        columnOperator: '_eq',
      };
    case 'column':
      return {
        name: selectedItem.name,
        typeName: selectedItem.name,
        type: 'column',
        columnOperator: '_eq',
      };
    case 'relationship':
      return {
        name: selectedItem.name,
        // for relationships the type name will be different from the name
        // for example if the relationship name is things the type name will be thing
        // therefore we need both to
        // 1. correctly display the relationship name in the permission i.e. things
        // 2. find the information needed about the type from the schema using the type name i.e. thing
        typeName: selectedItem?.meta?.type?.type,
        type: 'relationship',
        columnOperator: '_eq',
      };
    default:
      throw new Error('Case not handled');
  }
};

interface RenderJsonDisplayProps {
  dropDownState: { name: string; type: string };
}

/**
 *
 * needed to render the next level of the form differently depending on which item is selected
 */
const RenderJsonDisplay = (props: RenderJsonDisplayProps) => {
  const { dropDownState } = props;

  const isObjectType =
    dropDownState?.type === 'column' ||
    dropDownState?.type === 'relationship' ||
    dropDownState?.name === '_not';

  // if nothing is selected render a disabled input
  if (!dropDownState?.type) {
    return <CustomField.Input disabled />;
  }

  if (isObjectType) {
    return (
      <>
        <JsonItem text="{" />
      </>
    );
  }

  if (dropDownState?.name === '_and' || dropDownState?.name === '_or') {
    return (
      <>
        <JsonItem text="[" />
      </>
    );
  }

  return null;
};

interface Props {
  tableName: string;
  /**
   * The builder is a recursive structure
   * the nesting describes the level of the structure
   * so react hook form can correctly register the fields
   * e.g. ['filter', 'Title', '_eq'] would be registered as 'filter.Title._eq'
   */
  nesting: string[];
  schema: GraphQLSchema;
}

export const Builder = (props: Props) => {
  const { tableName, nesting, schema } = props;

  const { data } = useData({ tableName, schema });

  const { unregister, setValue, getValues } = useFormContext();

  // the selections from the dropdowns are stored on the form state under the key "operators"
  // this will be removed for submitting the form
  // and is generated from the permissions object when rendering the form from existing data
  const operatorsKey = createKey(['operators', ...nesting]);
  const dropDownState = getValues(operatorsKey);

  const permissionsKey = createKey([...nesting, dropDownState?.name]);
  const columnKey = createKey([
    ...nesting,
    dropDownState?.name,
    dropDownState?.columnOperator || '_eq',
  ]);

  const columnOperators = React.useMemo(() => {
    if (dropDownState?.name && dropDownState?.type === 'column' && schema) {
      return getColumnOperators({
        tableName,
        columnName: dropDownState.name,
        schema,
      });
    }

    return [];
  }, [tableName, dropDownState, schema]);

  const handleDropdownChange: React.ChangeEventHandler<HTMLSelectElement> =
    e => {
      const value = e.target.value;

      // as the form is populated a json object is built up
      // when the dropdown changes at a specific level
      // everything below that level needs to be removed
      // set value undefined is necessary to remove field arrays
      if (dropDownState?.name === '_and' || dropDownState?.name === '_or') {
        setValue(permissionsKey, undefined);
      }
      // when the dropdown changes both the permissions object
      // and operators object need to be unregistered below this level
      unregister(permissionsKey);
      unregister(operatorsKey);

      const newValue = getNewValues({ value, data });
      return setValue(operatorsKey, newValue);
    };

  const handleColumnChange: React.ChangeEventHandler<HTMLSelectElement> = e => {
    const target = e.target.value;

    // when the dropdown value changes the previous field needs to be unregistered
    // so it is removed from the form state
    unregister(columnKey);
    setValue(operatorsKey, {
      ...dropDownState,
      columnOperator: target,
    });
  };

  return (
    <div className="inline-flex pl-6 flex-col w-full border-dashed border-l border-gray-200">
      <div className="flex items-center">
        <CustomField.Select
          title="Relationship"
          value={dropDownState?.name || '-'}
          onChange={handleDropdownChange}
        >
          <option key="-" value="-">
            -
          </option>
          {Object.entries(data).map(([section, list]) => {
            return (
              <optgroup label={section} key={section}>
                {list.map(item => (
                  <option key={item.name} value={item.name}>
                    {item.name}
                  </option>
                ))}
              </optgroup>
            );
          })}
        </CustomField.Select>

        <JsonItem text=":" className="mr-4" />

        <RenderJsonDisplay dropDownState={dropDownState} />
      </div>

      {/* depending on the selection from the drop down different form elements need to render */}
      {/* for example if "_and" is selected a field array needs to render */}
      <RenderFormElement
        key={permissionsKey}
        columnKey={columnKey}
        tableName={tableName}
        dropDownState={dropDownState}
        columnOperators={columnOperators}
        handleColumnChange={handleColumnChange}
        nesting={nesting}
        schema={schema}
      />
    </div>
  );
};
