import React from 'react';
import { useFormContext, useWatch } from 'react-hook-form';
import { Button } from '@/new-components/Button';

import { Collapse } from '@/new-components/Collapse';

import { getEdForm } from '../../../components/Services/Data/utils';
import { useIsDisabled } from '../hooks/useIsDisabled';
import { QueryType } from '../types';

const getAccessText = (queryType: string) => {
  if (queryType === 'insert') {
    return 'to set input for';
  }

  if (queryType === 'select') {
    return 'to access';
  }

  return 'to update';
};

export interface ColumnPermissionsSectionProps {
  queryType: QueryType;
  roleName: string;
  columns?: string[];
}

const useStatus = (disabled: boolean) => {
  const { control } = useFormContext();
  const formColumns = useWatch({ control, name: 'columns' });

  if (!formColumns) {
    return { data: '', isError: true };
  }

  const columnValues = Object.values(formColumns);
  const selectedColumns = columnValues.filter(value => !!value);

  if (disabled) {
    return { data: 'Disabled: Set row permissions first', isError: false };
  }

  if (selectedColumns?.length === 0) {
    return { data: 'No columns', isError: false };
  }

  if (selectedColumns?.length === columnValues?.length) {
    return { data: 'All columns', isError: false };
  }

  return { data: 'Partial columns', isError: false };
};

// @todo
// this hasn't been fully implemented, it still needs computed columns adding
export const ColumnPermissionsSection: React.FC<ColumnPermissionsSectionProps> =
  ({ roleName, queryType, columns }) => {
    const { register, setValue } = useFormContext();

    // if no row permissions are selected selection should be disabled
    const disabled = useIsDisabled(queryType);

    const { data: status, isError } = useStatus(disabled);

    const onClick = () => {
      columns?.forEach(column => {
        const toggleAllOn = status !== 'All columns';
        // if status is not all columns: toggle all on
        // otherwise toggle all off
        setValue(`columns.${column}`, toggleAllOn);
      });
    };

    if (isError) {
      return <div>Error loading column permission data</div>;
    }

    return (
      <Collapse defaultOpen={!disabled}>
        <Collapse.Header
          title={`Column ${queryType} permissions`}
          tooltip={`Choose columns allowed to be ${getEdForm(queryType)}`}
          status={status}
          disabled={disabled}
          disabledMessage="Set row permissions first"
        />
        <Collapse.Content>
          <div
            title={disabled ? 'Set row permissions first' : ''}
            className="grid gap-2"
          >
            <div className="flex gap-2 items-center">
              <p>
                Allow role <strong>{roleName}</strong>{' '}
                {getAccessText(queryType)}
                &nbsp;
                <strong>columns</strong>:
              </p>
            </div>

            <fieldset className="flex gap-4">
              {columns?.map(fieldName => (
                <label key={fieldName} className="flex gap-2 items-center">
                  <input
                    type="checkbox"
                    title={disabled ? 'Set a row permission first' : ''}
                    disabled={disabled}
                    className="mt-0 rounded shadow-sm border border-gray-300 hover:border-gray-400 focus:ring-yellow-400"
                    {...register(`columns.${fieldName}`)}
                  />
                  <i>{fieldName}</i>
                </label>
              ))}
              <Button
                type="button"
                size="sm"
                title={disabled ? 'Set a row permission first' : ''}
                disabled={disabled}
                onClick={onClick}
                data-test="toggle-all-col-btn"
              >
                Toggle All
              </Button>
            </fieldset>
          </div>
          {/* {getExternalTablePermissionsMsg()} */}
        </Collapse.Content>
      </Collapse>
    );
  };

export default ColumnPermissionsSection;
