import { useMemo } from 'react';
import { useFieldArray } from 'react-hook-form';
import { Flex } from '@radix-ui/themes';
import { FaPlus } from 'react-icons/fa';
import {
  Button,
  CheckboxField,
  DropdownButton,
  DropdownMenu,
  IconButtonDelete,
  InputField,
  REACT_SELECT_FILTER_PROPS,
  ReactSelectField,
  Table,
  Text,
  TextAreaField,
  useConsoleForm,
} from '@hasura/shared/ui';
import type { Source } from '@hasura/shared/types';
import {
  getDatabaseMethods,
  useCreateTable,
  useGetTableSchemas,
  useSupportedDataTypes,
} from '@hasura/metadata/data-source';
import { Section } from '../ModifyTable/parts';
import { ForeignKeysSection } from './ForeignKeysSection';
import { UniqueKeysSection } from './UniqueKeysSection';
import { CheckConstraintsSection } from './CheckConstraintsSection';
import {
  addTableDefaultValues,
  addTableSchema,
  AddTableFormValues,
  defaultColumn,
  formValuesToCreateTableArgs,
  frequentlyUsedColumnToFormColumn,
} from './schema';

export type AddTableFormProps = {
  source: Source;
  targetSchema: string | null;
  onCreated: (table: { name: string; schema: string }) => void;
};

const tableColumns = [
  'Name',
  'Type',
  'Default',
  'Nullable',
  'Array',
  'Unique',
  'Primary Key',
  '',
];
export const AddTableForm = ({
  source,
  targetSchema,
  onCreated,
}: AddTableFormProps) => {
  const { data: dataTypeMap } = useSupportedDataTypes(source.kind);
  const { data: schemaList } = useGetTableSchemas({ source });

  const typeOptions = useMemo(() => {
    const allTypes = dataTypeMap ? Object.values(dataTypeMap).flat() : [];
    return Array.from(new Set(allTypes)).map((type) => ({
      value: type,
      label: type,
    }));
  }, [dataTypeMap]);

  const schemaOptions = useMemo(
    () =>
      (schemaList ?? []).map((schema) => ({ value: schema, label: schema })),
    [schemaList],
  );

  const {
    Form,
    methods: { control },
  } = useConsoleForm({
    schema: addTableSchema,
    options: {
      defaultValues: {
        ...addTableDefaultValues,
        schema: targetSchema ?? '',
      },
    },
  });

  const columns = useFieldArray({ control, name: 'columns' });

  const frequentlyUsedColumns = (
    getDatabaseMethods(source.kind).config?.getFrequentlyUsedColumns?.() ?? []
  ).filter((c) => c.validFor.includes('add'));

  const { mutate: createTable, isPending } = useCreateTable({
    onSuccess: (_data, variables) => {
      const table = variables.args.table as { name: string; schema: string };
      onCreated(table);
    },
  });

  const onSubmit = (values: AddTableFormValues) => {
    createTable({
      source,
      args: formValuesToCreateTableArgs(values),
    });
  };

  return (
    <Form onSubmit={onSubmit}>
      <div>
        <div className="sm:w-1/2">
          <ReactSelectField
            name="schema"
            label="Schema"
            placeholder="Select a schema"
            options={schemaOptions}
            selectProps={REACT_SELECT_FILTER_PROPS}
          />
          <InputField
            name="name"
            fieldProps={{ placeholder: 'table_name' }}
            label="Table name"
          />
        </div>

        <Section headerText="Columns">
          <Table.Root variant="ghost" size="1">
            <Table.Header>
              <Table.Row>
                {tableColumns.map((header) => (
                  <Table.RowHeaderCell key={header}>
                    <Text weight="bold">{header}</Text>
                  </Table.RowHeaderCell>
                ))}
              </Table.Row>
            </Table.Header>
            <Table.Body>
              {columns.fields.map((field, index) => (
                <Table.Row key={field.id}>
                  <Table.Cell>
                    <InputField
                      name={`columns.${index}.name`}
                      fieldProps={{ placeholder: 'column_name' }}
                      noErrorPlaceholder
                    />
                  </Table.Cell>
                  <Table.Cell>
                    <ReactSelectField
                      name={`columns.${index}.type`}
                      placeholder="Select a type"
                      options={typeOptions}
                      selectProps={REACT_SELECT_FILTER_PROPS}
                      noErrorPlaceholder
                    />
                  </Table.Cell>
                  <Table.Cell>
                    <InputField
                      name={`columns.${index}.default`}
                      fieldProps={{ placeholder: '(optional)' }}
                      noErrorPlaceholder
                    />
                  </Table.Cell>
                  <Table.Cell width="64px">
                    <div className="pt-2">
                      <CheckboxField
                        name={`columns.${index}.nullable`}
                        noErrorPlaceholder
                      />
                    </div>
                  </Table.Cell>
                  <Table.Cell width="64px">
                    <div className="pt-2">
                      <CheckboxField
                        name={`columns.${index}.array`}
                        noErrorPlaceholder
                      />
                    </div>
                  </Table.Cell>
                  <Table.Cell width="64px">
                    <div className="pt-2">
                      <CheckboxField
                        name={`columns.${index}.unique`}
                        noErrorPlaceholder
                      />
                    </div>
                  </Table.Cell>
                  <Table.Cell width="92px">
                    <div className="pt-2">
                      <CheckboxField
                        name={`columns.${index}.isPrimaryKey`}
                        noErrorPlaceholder
                      />
                    </div>
                  </Table.Cell>
                  <Table.Cell align="left">
                    <div className="pt-2">
                      <IconButtonDelete
                        type="button"
                        aria-label={`Remove column ${index + 1}`}
                        disabled={columns.fields.length === 1}
                        onClick={() => columns.remove(index)}
                      />
                    </div>
                  </Table.Cell>
                </Table.Row>
              ))}
            </Table.Body>
          </Table.Root>
          <Flex align="center" gap="2" className="mt-4">
            <Button
              type="button"
              mode="default"
              size="1"
              leftIcon={FaPlus}
              onClick={() => columns.append({ ...defaultColumn })}
            >
              Add Column
            </Button>
            {frequentlyUsedColumns.length > 0 && (
              <DropdownButton
                mode="default"
                size="1"
                items={frequentlyUsedColumns.map((preset) => (
                  <DropdownMenu.Item
                    key={`${preset.name}-${preset.type}`}
                    onSelect={() =>
                      columns.append(frequentlyUsedColumnToFormColumn(preset))
                    }
                  >
                    {preset.name} &middot; {preset.typeText}
                  </DropdownMenu.Item>
                ))}
              >
                Frequently used columns
              </DropdownButton>
            )}
          </Flex>
        </Section>

        <CheckConstraintsSection />

        <UniqueKeysSection />

        <ForeignKeysSection source={source} />

        <div className="mb-4">
          <TextAreaField
            name="comment"
            label="Table comment"
            placeholder="(optional)"
            noErrorPlaceholder
          />
        </div>

        <Button type="submit" mode="primary" loading={isPending}>
          Add Table
        </Button>
      </div>
    </Form>
  );
};

export default AddTableForm;
