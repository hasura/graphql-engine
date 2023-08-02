import React from 'react';
import { useFormContext, useFieldArray, useWatch } from 'react-hook-form';

import { Button } from '../../../../new-components/Button';
import { Collapse } from '../../../../new-components/deprecated';

import { getIngForm } from '../../../../components/Services/Data/utils';
import { useIsDisabled } from '../hooks/useIsDisabled';
import { QueryType } from '../../types';

interface PresetsRowProps {
  id: number;
  columns?: string[];
  allDisabled: boolean;
  remove: () => void;
}

const className =
  'block w-full h-input px-md shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-none focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400';

const PresetsRow: React.FC<PresetsRowProps> = ({
  id,
  columns,
  allDisabled,
  remove,
}) => {
  const { register, watch } = useFormContext();

  const watched: Preset = watch(`presets.${id}`);
  const disabled = allDisabled || watched.columnName === 'default_column';

  return (
    <div className="grid grid-cols-5 gap-4">
      <div>
        <select
          aria-label="select column"
          className={className}
          defaultValue={watched.columnName}
          disabled={allDisabled}
          title={allDisabled ? 'Set a row permission first' : ''}
          data-index-id={id}
          data-test={`column-presets-column-${id}`}
          {...register(`presets.${id}.columnName`)}
        >
          <option key="default_column_name" value="default" disabled>
            Column Name
          </option>

          {columns?.map(columnName => (
            <option key={columnName} value={columnName}>
              {columnName}
            </option>
          ))}
        </select>
      </div>

      <div>
        <select
          id="presetType"
          className={className}
          disabled={disabled}
          title={disabled ? 'Choose column fist' : ''}
          data-index-id={id}
          data-test={`column-presets-column-${id}`}
          {...register(`presets.${id}.presetType`)}
        >
          <option key="default_preset_type" value="default" disabled>
            Select Preset Type
          </option>

          {['static', 'from session variable'].map(type => (
            <option key={type} value={type}>
              {type}
            </option>
          ))}
        </select>
      </div>

      <div>
        <input
          id="columnValue"
          type="text"
          className={className}
          placeholder="Column value"
          disabled={disabled}
          {...register(`presets.${id}.columnValue`)}
        />
      </div>

      <div className="flex items-center">
        <p>
          {watched.presetType !== 'static'
            ? 'e.g. X-Hasura-User-Id'
            : 'e.g. false, 1, some-text'}
        </p>
      </div>

      {watched.columnName !== 'default' && (
        <div className="flex items-center">
          <Button type="button" size="sm" mode="destructive" onClick={remove}>
            Delete
          </Button>
        </div>
      )}
    </div>
  );
};

export interface ColumnPresetsSectionProps {
  queryType: QueryType;
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
    .filter(columnName => columnName !== 'default');

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
      controlledFields[controlledFields?.length - 1]?.columnName !== 'default';
    const allColumnsSet = controlledFields?.length === columns?.length;

    if (finalRowIsNotDefault && !allColumnsSet) {
      append({
        columnName: 'default',
        presetType: 'static',
        columnValue: '',
      });
    }
  }, [controlledFields, columns?.length, append]);

  return (
    <Collapse defaultOpen={presets?.length > 0 && !disabled}>
      <Collapse.Header
        title="Column presets"
        tooltip={`Set static values or session variables as pre-determined values
              for columns while ${getIngForm(queryType)}`}
        status={status}
        disabled={disabled}
        disabledMessage={status}
      />
      <Collapse.Content>
        <div className="grid gap-4">
          {controlledFields.map((field, index) => {
            // remove current preset from columns to remove
            const columnsToRemove = controlledFields
              .map(preset => preset?.columnName)
              .filter(preset => preset !== presets[index]?.columnName);

            // remove other presets from selectable columns
            const selectableColumns = columns?.filter(
              column => !columnsToRemove.includes(column)
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
        </div>
      </Collapse.Content>
    </Collapse>
  );
};

export default ColumnPresetsSection;
