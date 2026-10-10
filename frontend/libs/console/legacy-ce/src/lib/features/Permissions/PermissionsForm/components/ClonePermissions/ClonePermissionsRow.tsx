import React from 'react';
import {
  Button,
  ReactSelectField,
  ReactSelectOptionType,
  SelectField,
} from '@hasura/shared/ui';
import { Table } from '@hasura/shared/types';
import { getTableDisplayName } from '@hasura/shared/utils';
import { Flex, Grid } from '@radix-ui/themes';
import { FilterOptionOption } from 'react-select';
import { ClonePermissionItem } from '@hasura/metadata/api';

interface ClonePermissionsRowProps {
  id: number;
  tables: Table[];
  permission: ClonePermissionItem;
  queryTypes: string[];
  roleNames: string[];
  remove: () => void;
  disabled: boolean;
}

const filterOption = (
  option: FilterOptionOption<ReactSelectOptionType>,
  inputValue: string,
) => {
  return option.label.toLowerCase().includes(inputValue.toLowerCase());
};

export const formKey = 'clonePermissions';

const ClonePermissionsRow: React.FC<ClonePermissionsRowProps> = ({
  id,
  tables,
  queryTypes,
  roleNames,
  remove,
  permission,
  disabled,
}) => {
  return (
    <Grid
      columns={{
        initial: '1',
        sm: '4',
      }}
      gap="4"
      className="w-full"
    >
      <ReactSelectField
        name={`${formKey}.${id}.table`}
        placeholder="Table Name"
        noErrorPlaceholder
        options={
          tables?.map((table) => {
            const tableName = getTableDisplayName(table);
            return {
              label: tableName,
              value: table,
            };
          }) ?? []
        }
        disabled={disabled}
        selectProps={{
          isSearchable: true,
          filterOption,
        }}
      />

      <SelectField
        name={`${formKey}.${id}.queryType`}
        placeholder="Select Action"
        full
        noErrorPlaceholder
        disabled={disabled}
        options={queryTypes.map((qt) => ({
          label: qt,
          value: qt,
        }))}
      />

      <ReactSelectField
        name={`${formKey}.${id}.roleName`}
        placeholder="Select Role"
        noErrorPlaceholder
        options={roleNames.map((value) => ({
          label: value,
          value,
        }))}
        disabled={disabled}
        selectProps={{
          isSearchable: true,
          filterOption,
        }}
      />
      {Boolean(permission?.table) &&
        permission?.queryType !== '' &&
        permission?.roleName !== '' && (
          <Flex align="start">
            <Button
              type="button"
              size="sm"
              mode="destructive"
              onClick={remove}
              disabled={disabled}
            >
              Delete
            </Button>
          </Flex>
        )}
    </Grid>
  );
};

export default ClonePermissionsRow;
