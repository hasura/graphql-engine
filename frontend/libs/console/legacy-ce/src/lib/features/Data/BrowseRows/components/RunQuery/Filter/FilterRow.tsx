import {
  CodeEditorField,
  DateTimePickerField,
  getDialogPortalTarget,
  IconButton,
  InputField,
  ReactSelectField,
  ReactSelectOptionType,
  SwitchField,
  Text,
} from '@hasura/shared/ui';
import { useEffect } from 'react';
import { Controller, useFormContext } from 'react-hook-form';
import { FaTimesCircle } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import { createFilter } from 'react-select';
import { Operator, TableColumn } from '@hasura/metadata/data-source';
import { isArrayString, isJsonString } from '@hasura/shared/utils';
import { format } from 'date-fns';
import { ArrayInput } from '../../../../InsertRow/components/ArrayInput';
import { hasTimeZone } from '../../../../InsertRow/components/ColumnRowInput.utils';

export type FilterValueKind = 'list' | 'boolean' | 'json' | 'datetime' | 'text';

const BUILT_IN_SCALARS = ['String', 'Int', 'Float', 'Boolean', 'ID'];

/** Operator names follow the naming convention, e.g. `_is_null` or `_isNull`. */
const isNullCheck = (operatorName?: string) =>
  !!operatorName && /^_is_?null$/i.test(operatorName);

/**
 * The kind of value a filter takes, from the operand type of its operator,
 * e.g. a list for `_in`, a boolean for `_is_null` or a JSON document for
 * `_contains` on a `jsonb` column. Without the operand types, an operator
 * compares values of the column type.
 */
export const getFilterValueKind = (
  operatorName?: string,
  { operator, column }: { operator?: Operator; column?: TableColumn } = {},
): FilterValueKind => {
  const valueType = operator?.valueType;

  if (
    operator?.isListValue ??
    (operatorName === '_in' || operatorName === '_nin')
  )
    return 'list';
  if (valueType === 'Boolean' || isNullCheck(operatorName)) return 'boolean';
  if (
    column?.consoleDataType === 'json' &&
    (!valueType || !BUILT_IN_SCALARS.includes(valueType))
  )
    return 'json';
  if (column?.consoleDataType === 'boolean' && !valueType) return 'boolean';
  // a Data Connector datetime is a `String` in GraphQL
  if (
    column?.consoleDataType === 'timestamp' &&
    (!valueType || !['Int', 'Float', 'ID'].includes(valueType))
  )
    return 'datetime';
  return 'text';
};

const isValidDate = (value: unknown): value is Date =>
  value instanceof Date && !Number.isNaN(value.getTime());

/**
 * Converts the form value of a filter to the value it is queried with: a
 * JSON document is parsed, and a date is formatted for the column type, as
 * UTC with a time zone, or as the local time without one.
 */
export const toFilterValue = (
  value: unknown,
  kind: FilterValueKind,
  column?: TableColumn,
): unknown => {
  if (kind === 'json' && typeof value === 'string' && isJsonString(value))
    return JSON.parse(value);
  if (kind === 'datetime' && isValidDate(value))
    return column && hasTimeZone(column.dataType)
      ? value.toISOString()
      : format(value, "yyyy-MM-dd'T'HH:mm:ss");
  return value;
};

const isListValue = (value: unknown) =>
  Array.isArray(value) || (typeof value === 'string' && isArrayString(value));

const isObjectValue = (value: unknown) =>
  !!value &&
  typeof value === 'object' &&
  !Array.isArray(value) &&
  !(value instanceof Date);

const noop = () => undefined;

/** Keeps a current value that is no longer in the enum table selectable. */
const toEnumOptions = (
  values: string[],
  current: unknown,
): ReactSelectOptionType[] => {
  const currentValues = (Array.isArray(current) ? current : [current]).filter(
    (value): value is string =>
      typeof value === 'string' && !!value && !values.includes(value),
  );
  return [...currentValues, ...values].map((value) => ({
    label: value,
    value,
  }));
};

type FilterRowProps = {
  columnOptions: ReactSelectOptionType[];
  /** the table columns, whose types pick the value input */
  columns: TableColumn[];
  onRemove: () => void;
  /** operators of a column whose operators aren't known */
  operators: Operator[];
  /** the operators each column supports, by column name */
  columnOperators?: Record<string, Operator[]>;
  /** the allowed values of the columns that reference an enum table */
  enumColumnValues?: Record<string, string[]>;
  name: string;
  defaultValues?: Record<string, string>;
  disabled?: boolean;
};

const toOperatorOption = (operator: Operator): ReactSelectOptionType => ({
  label: `[${operator.value}] ${operator.name}`,
  value: operator.value,
});

export const FilterRow = ({
  columnOptions,
  columns,
  onRemove,
  operators,
  columnOperators,
  enumColumnValues,
  name,
  defaultValues,
  disabled,
}: FilterRowProps) => {
  const { control, setValue, watch } = useFormContext();
  const localColumn = watch(`${name}.column`);
  const localOperator = watch(`${name}.operator`);
  const localValue = watch(`${name}.value`);

  // e.g. no `<` on a text column and no `like` on a number column
  const supportedOperators = localColumn
    ? columnOperators?.[localColumn]
    : undefined;
  const availableOperators = supportedOperators ?? operators;
  const operatorOptions = availableOperators.map(toOperatorOption);
  const enumValues = localColumn ? enumColumnValues?.[localColumn] : undefined;
  const selectedColumn = columns.find((column) => column.name === localColumn);
  const valueKind = getFilterValueKind(localOperator, {
    operator: availableOperators.find(
      (operator) => operator.value === localOperator,
    ),
    column: selectedColumn,
  });

  /**
   * Set the operator to the first one when it is empty, or when the selected
   * column doesn't support it
   */
  useEffect(() => {
    const isSupported =
      !supportedOperators ||
      supportedOperators.some((operator) => operator.value === localOperator);
    if ((!localOperator || !isSupported) && operatorOptions[0]?.value) {
      setValue(`${name}.operator`, operatorOptions[0].value);
    }
  }, [localOperator, supportedOperators, operatorOptions, setValue, name]);

  /**
   * Fit the value to the input of the operator, e.g. after switching from
   * `_in` to `_eq`, or for a value restored from the URL (`["a"]`, `true`)
   */
  useEffect(() => {
    const fieldName = `${name}.value`;
    if (valueKind === 'boolean' && typeof localValue !== 'boolean') {
      // a null check defaults to `IS NULL`
      setValue(fieldName, localValue !== 'false');
    } else if (valueKind === 'list' && !isListValue(localValue)) {
      setValue(fieldName, []);
    } else if (valueKind === 'list' && typeof localValue === 'string') {
      // the list text of a filter restored from the URL
      setValue(fieldName, JSON.parse(localValue));
    } else if (valueKind === 'datetime') {
      // the picker holds a date: from the URL text, or now by default
      if (!isValidDate(localValue)) {
        const date =
          typeof localValue === 'string' && localValue
            ? new Date(localValue)
            : undefined;
        setValue(fieldName, isValidDate(date) ? date : new Date());
      }
    } else if (valueKind === 'json' && isObjectValue(localValue)) {
      // the editor holds the JSON text of a document restored from the URL
      setValue(fieldName, JSON.stringify(localValue, null, 2));
    } else if (
      (valueKind === 'text' || valueKind === 'json') &&
      typeof localValue !== 'string' &&
      typeof localValue !== 'number'
    ) {
      setValue(fieldName, '');
    }
  }, [valueKind, localValue, setValue, name]);

  /**
   * Set the default value into the input field depending on the operator type
   */
  useEffect(() => {
    if (localOperator && !localValue && defaultValues?.[localOperator]) {
      setValue(`${name}.value`, defaultValues?.[localOperator]);
    }
  }, [localOperator, localValue, defaultValues, setValue, name]);

  const defaultSelectProps = {
    filterOption: createFilter({
      ignoreCase: true,
      matchFrom: 'any',
    }),
    menuPortalTarget: getDialogPortalTarget(),
    disabled,
  };

  return (
    <Flex data-testid={`${name}-filter-row`} align="start" gap="2">
      <div className="sm:w-4/12">
        <ReactSelectField
          name={`${name}.column`}
          options={columnOptions}
          placeholder="Select a column"
          data-test={`${name}.column`}
          dataTest={`${name}.column`}
          noErrorPlaceholder
          selectProps={defaultSelectProps}
        />
      </div>

      <div className="sm:w-4/12">
        <ReactSelectField
          name={`${name}.operator`}
          options={operatorOptions}
          placeholder="Select an operator"
          data-test={`${name}.operator`}
          noErrorPlaceholder
          selectProps={defaultSelectProps}
        />
      </div>

      <div className="sm:w-4/12">
        {enumValues && (valueKind === 'list' || valueKind === 'text') ? (
          <ReactSelectField
            name={`${name}.value`}
            options={toEnumOptions(enumValues, localValue)}
            multi={valueKind === 'list'}
            placeholder="Select a value"
            noErrorPlaceholder
            selectProps={defaultSelectProps}
          />
        ) : valueKind === 'list' ? (
          <Controller
            name={`${name}.value`}
            control={control}
            render={({ field }) => (
              <ArrayInput
                name={`${name}.value`}
                initialValue={field.value}
                disabled={disabled}
                onValueChange={(e) => field.onChange(e.target.value)}
                onCheckValueRadio={noop}
              />
            )}
          />
        ) : valueKind === 'boolean' ? (
          <SwitchField
            name={`${name}.value`}
            disabled={disabled}
            noErrorPlaceholder
          >
            {isNullCheck(localOperator)
              ? localValue === false
                ? 'is not null'
                : 'is null'
              : String(localValue !== false)}
          </SwitchField>
        ) : valueKind === 'datetime' ? (
          isValidDate(localValue) && (
            <Flex direction="column" gap="1">
              <DateTimePickerField
                name={`${name}.value`}
                disabled={disabled}
                noErrorPlaceholder
              />
              {/* the picker shows the local time, the rows and filter tags
                  show the value it is queried with */}
              <Text size="1" color="gray" data-testid={`${name}.value-iso`}>
                {String(toFilterValue(localValue, valueKind, selectedColumn))}
              </Text>
            </Flex>
          )
        ) : valueKind === 'json' ? (
          <CodeEditorField
            name={`${name}.value`}
            placeholder='{"key": "value"}'
            editorProps={{ mode: 'json', width: 'auto' }}
            editorOptions={{
              minLines: 1,
              maxLines: 8,
              showLineNumbers: false,
              showGutter: false,
              showFoldWidgets: false,
              showInvisibles: false,
            }}
            disabled={disabled}
            noErrorPlaceholder
          />
        ) : (
          <InputField
            name={`${name}.value`}
            fieldProps={{ placeholder: '-- value --' }}
            data-test={`${name}.value`}
            noErrorPlaceholder
          />
        )}
      </div>
      <div className="pt-2">
        <IconButton
          color="indigo"
          variant="ghost"
          radius="full"
          disabled={false}
          onClick={onRemove}
          data-testid={`${name}.remove`}
        >
          <FaTimesCircle />
        </IconButton>
      </div>
    </Flex>
  );
};
