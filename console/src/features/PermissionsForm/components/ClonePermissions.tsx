import React from 'react';
import { useFormContext, useFieldArray } from 'react-hook-form';

import { Button } from '@/new-components/Button';
import { Collapse } from '@/new-components/Collapse';
import { useIsDisabled } from '../hooks/useIsDisabled';
import { QueryType } from '../types';

interface ClonePermissionsRowProps {
  id: number;
  tables: string[];
  currentQueryType: QueryType;
  queryTypes: string[];
  roleNames: string[];
  remove: () => void;
}

const className =
  'block w-full h-input px-md shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-none focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400';

export const ClonePermissionsRow: React.FC<ClonePermissionsRowProps> = ({
  id,
  tables,
  currentQueryType,
  queryTypes,
  roleNames,
  remove,
}) => {
  const { register, watch } = useFormContext();

  const formKey = 'clonePermissions';
  const watched: ClonePermission = watch(`${formKey}.${id}`);

  const allDisabled = useIsDisabled(currentQueryType as QueryType);

  return (
    <div className="width-full grid grid-cols-4 gap-4">
      <div>
        <select
          className={className}
          disabled={allDisabled}
          title={allDisabled ? 'Set a row permission first' : ''}
          {...register(`${formKey}.${id}.tableName`)}
        >
          <option key="default_table_name" value="" disabled>
            Table Name
          </option>

          {tables?.map(tableName => (
            <option key={tableName} value={tableName}>
              {tableName}
            </option>
          ))}
        </select>
      </div>

      <div>
        <select
          className={className}
          disabled={allDisabled}
          title={allDisabled ? 'Set a row permission first' : ''}
          {...register(`${formKey}.${id}.queryType`)}
        >
          <option key="default_query_type" value="" disabled>
            Select Action
          </option>

          {queryTypes.map(type => (
            <option key={type} value={type}>
              {type}
            </option>
          ))}
        </select>
      </div>

      <div>
        <select
          className={className}
          disabled={allDisabled}
          title={allDisabled ? 'Set a row permission first' : ''}
          {...register(`${formKey}.${id}.roleName`)}
        >
          <option key="default_role_type" value="" disabled>
            Select Role
          </option>

          {roleNames.map(type => (
            <option key={type} value={type}>
              {type}
            </option>
          ))}
        </select>
      </div>
      {watched.tableName !== '' &&
        watched.queryType !== '' &&
        watched.roleName !== '' && (
          <div className="flex items-center">
            <Button type="button" size="sm" mode="destructive" onClick={remove}>
              Delete
            </Button>
          </div>
        )}
    </div>
  );
};

export interface ClonePermissionsSectionProps {
  queryType: string;
  tables: string[];
  supportedQueryTypes: string[];
  roles: string[];
  defaultOpen?: boolean;
}

export interface ClonePermission {
  id: number;
  tableName: string;
  queryType: string;
  roleName: string;
}

export const ClonePermissionsSection: React.FC<ClonePermissionsSectionProps> = ({
  queryType,
  tables,
  supportedQueryTypes,
  roles,
  defaultOpen,
}) => {
  const { control, watch } = useFormContext();

  const disabled = useIsDisabled(queryType as QueryType);

  const { fields, append, remove } = useFieldArray({
    control,
    name: 'clonePermissions',
  });

  const watched: ClonePermission[] = watch('clonePermissions');
  const controlledFields = fields.map((field, index) => {
    return {
      ...field,
      ...watched[index],
    };
  });

  React.useEffect(() => {
    const finalRow = controlledFields[controlledFields.length - 1];

    const finalRowIsNotEmpty =
      finalRow?.tableName !== '' &&
      finalRow?.queryType !== '' &&
      finalRow?.roleName !== '';

    if (finalRowIsNotEmpty) {
      append({
        tableName: '',
        queryType: '',
        roleName: '',
      } as ClonePermission);
    }
  }, [controlledFields, append]);

  return (
    <Collapse defaultOpen={defaultOpen}>
      <Collapse.Header
        title="Clone permissions"
        tooltip="Apply same permissions to other tables/actions/roles"
      />
      <Collapse.Content>
        <div
          title={disabled ? 'Set a row permission first' : ''}
          className="width-full grid gap-4 justify-start"
        >
          <p>Apply permissions for:</p>

          {controlledFields.map((field, index) => {
            return (
              <ClonePermissionsRow
                key={field.id}
                id={index}
                tables={tables}
                currentQueryType={queryType as QueryType}
                queryTypes={supportedQueryTypes}
                roleNames={roles}
                remove={() => remove(index)}
              />
            );
          })}
          <p>
            <strong>Note:</strong> While applying permissions for other tables,
            the column permissions and presets will be ignored
          </p>
        </div>
      </Collapse.Content>
    </Collapse>
  );
};

export default ClonePermissionsSection;
