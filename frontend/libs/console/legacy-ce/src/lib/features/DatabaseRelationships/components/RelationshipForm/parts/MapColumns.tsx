import { FieldError, useFieldArray, useFormContext } from 'react-hook-form';
import {
  Button,
  getDialogPortalTarget,
  IconButton,
  IndicatorCard,
  ReactSelectField,
  Text,
} from '@hasura/shared/ui';
import {
  FaArrowAltCircleLeft,
  FaArrowAltCircleRight,
  FaArrowRight,
  FaTimesCircle,
} from 'react-icons/fa';
import { TableColumn } from '@hasura/metadata/data-source';
import get from 'lodash/get';
import { Flex } from '@radix-ui/themes';
import { createFilter } from 'react-select';

type SchemaValue = {
  from?: string;
  to?: string;
};

type Schema = Record<string, SchemaValue[]>;

export const MapColumns = ({
  name,
  sourceTableColumns,
  targetTableColumns,
}: {
  name: string;
  sourceTableColumns: TableColumn[];
  targetTableColumns: TableColumn[];
}) => {
  const { fields, append } = useFieldArray<Schema>({ name });

  const {
    watch,
    setValue,
    formState: { errors },
  } = useFormContext<Schema>();

  const maybeError = get(errors, name) as unknown as FieldError;

  const columnMappings: SchemaValue[] = watch(name);
  const commonSelectProps = {
    filterOption: createFilter({
      ignoreCase: true,
      matchFrom: 'any',
    }),
    menuPortalTarget: getDialogPortalTarget(),
  };

  return (
    <div className="my-4">
      <Flex align="center" justify="between" className="mb-2">
        <Flex align="center" gap="2" className="w-5/12">
          <Text>Source Column</Text>
          <Text color="green">
            <FaArrowAltCircleRight />
          </Text>
        </Flex>
        <Flex align="center" gap="2" className="w-5/12">
          <Text>Reference Column</Text>
          <Text color="purple">
            <FaArrowAltCircleLeft />
          </Text>
        </Flex>
      </Flex>
      {fields.map((field, index) => {
        return (
          <Flex
            align="center"
            justify="between"
            gap="2"
            className="mb-2"
            key={`${index}_column_map_row`}
          >
            <Flex align="center" justify="between" className="w-5/12">
              <ReactSelectField
                options={sourceTableColumns.map((column) => ({
                  label: column.name,
                  value: column.name,
                }))}
                name={`${name}.${index}.from`}
                disabled={!sourceTableColumns?.length}
                placeholder="Select source column"
                noErrorPlaceholder
                selectProps={commonSelectProps}
              />
            </Flex>
            <FaArrowRight className="fill-muted" />
            <Flex align="center" justify="between" className="w-5/12" gap="2">
              <ReactSelectField
                options={(targetTableColumns ?? []).map((column) => ({
                  label: column.name,
                  value: column.name,
                }))}
                name={`${name}.${index}.to`}
                disabled={!targetTableColumns?.length}
                placeholder="Select reference column"
                noErrorPlaceholder
                selectProps={commonSelectProps}
              />
              <IconButton
                type="button"
                mode="primary"
                variant="ghost"
                radius="full"
                onClick={() => {
                  setValue(
                    name,
                    columnMappings.filter((_, i) => index !== i),
                  );
                }}
              >
                <FaTimesCircle />
              </IconButton>
            </Flex>
          </Flex>
        );
      })}
      {maybeError && (
        <IndicatorCard status="negative" showIcon>
          {maybeError.message}
        </IndicatorCard>
      )}
      <div className="my-4">
        <Button
          type="button"
          size="1"
          mode="default"
          onClick={() => append({})}
          disabled={!targetTableColumns?.length || !sourceTableColumns?.length}
        >
          Add New Mapping
        </Button>
      </div>
    </div>
  );
};
