import React from 'react';
import { useFormContext, useFieldArray, useWatch } from 'react-hook-form';
import { Flex, Grid } from '@radix-ui/themes';
import {
  Button,
  Collapsible,
  CollapsibleHeader,
  InputField,
  SelectField,
  Text,
} from '@hasura/shared/ui';
import { useIsDisabled } from '../hooks/useIsDisabled';
import { DataQueryType } from '@hasura/shared/types';
import { getIngForm } from '@hasura/shared/utils';

interface PresetsRowProps {
  id: number;
  columns?: string[];
  allDisabled: boolean;
  remove: () => void;
}

const PRESET_TYPE_OPTIONS = ['static', 'from session variable'].map(
  (value) => ({
    label: value,
    value,
  }),
);

const PresetsRow: React.FC<PresetsRowProps> = ({
  id,
  columns,
  allDisabled,
  remove,
}) => {
  const { watch } = useFormContext();

  const watched: Preset = watch(`presets.${id}`);
  const disabled = allDisabled || watched.columnName === 'default_column';

  return (
    <Flex align="center" gap="4">
      <div className="md:w-3/12">
        <SelectField
          full
          name={`presets.${id}.columnName`}
          aria-label="select column"
          defaultValue={watched.columnName}
          disabled={allDisabled}
          data-index-id={id}
          data-test={`column-presets-column-${id}`}
          placeholder={
            allDisabled ? 'Set a row permission first' : 'Column Name'
          }
          noErrorPlaceholder
          options={
            columns?.map((columnName) => ({
              label: columnName,
              value: columnName,
            })) ?? []
          }
        />
      </div>

      <div className="md:w-3/12">
        <SelectField
          full
          disabled={disabled}
          data-index-id={id}
          data-test={`column-presets-column-${id}`}
          placeholder={disabled ? 'Choose column first' : 'Select Preset Type'}
          name={`presets.${id}.presetType`}
          options={PRESET_TYPE_OPTIONS}
          noErrorPlaceholder
        />
      </div>

      <div className="md:w-3/12">
        <InputField
          noErrorPlaceholder
          name={`presets.${id}.columnValue`}
          fieldProps={{
            placeholder: 'Column value',
            disabled,
            full: true,
          }}
        />
      </div>

      <Flex align="center" className="md:w-2/12">
        <Text>
          {watched.presetType !== 'static'
            ? 'e.g. X-Hasura-User-Id'
            : 'e.g. false, 1, some-text'}
        </Text>
      </Flex>

      {watched.columnName !== 'default' && (
        <Flex align="center" className="md:w-1/12">
          <Button type="button" size="sm" mode="destructive" onClick={remove}>
            Delete
          </Button>
        </Flex>
      )}
    </Flex>
  );
};

export interface ColumnPresetsSectionProps {
  queryType: DataQueryType;
  columns?: string[];
}

export interface Preset {
  id: number;
  columnName: string;
  presetType: string;
  columnValue: string | number;
}

const useStatus = (disabled: boolean) => {
  const { control } = useFormContext();
  const presets: Preset[] = useWatch({ control, name: 'presets' });

  if (disabled) {
    return 'Disabled: Set row permissions first';
  }

  const columnNames = presets
    ?.map(({ columnName }) => columnName)
    .filter((columnName) => columnName !== 'default');

  if (!columnNames?.length) {
    return 'No Presets';
  }

  return `Presets: ${columnNames.join(', ')}`;
};

export const ColumnPresetsSection: React.FC<ColumnPresetsSectionProps> = ({
  queryType,
  columns,
}) => {
  const { control, watch } = useFormContext();

  const disabled = useIsDisabled(queryType);

  const status = useStatus(disabled);

  const { fields, append, remove } = useFieldArray({
    control,
    name: 'presets',
  });

  const presets: Preset[] = watch('presets');
  const controlledFields = fields.map((field, index) => {
    return {
      ...field,
      ...presets[index],
    };
  });

  React.useEffect(() => {
    const finalRowIsNotDefault =
      controlledFields[controlledFields?.length - 1]?.columnName;
    const allColumnsSet = controlledFields?.length === columns?.length;

    if (finalRowIsNotDefault && !allColumnsSet) {
      append({
        columnName: '',
        presetType: 'static',
        columnValue: '',
      });
    }
  }, [controlledFields, columns?.length, append]);

  return (
    <Collapsible
      defaultOpen={presets?.length > 0 && !disabled}
      disabled={disabled}
      triggerChildren={
        <CollapsibleHeader
          title="Column presets"
          tooltip={`Set static values or session variables as pre-determined values
              for columns while ${getIngForm(queryType)}`}
          status={status}
        />
      }
    >
      <Grid gap="4">
        {controlledFields.map((field, index) => {
          // remove current preset from columns to remove
          const columnsToRemove = controlledFields
            .map((preset) => preset?.columnName)
            .filter((preset) => preset !== presets[index]?.columnName);

          // remove other presets from selectable columns
          const selectableColumns = columns?.filter(
            (column) => !columnsToRemove.includes(column),
          );

          return (
            <PresetsRow
              key={field.id}
              id={index}
              allDisabled={disabled}
              columns={selectableColumns}
              remove={() => remove(index)}
            />
          );
        })}
      </Grid>
    </Collapsible>
  );
};

export default ColumnPresetsSection;
