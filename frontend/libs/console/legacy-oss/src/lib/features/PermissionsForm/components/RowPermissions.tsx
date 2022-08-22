import React from 'react';
import { useFormContext, Controller } from 'react-hook-form';

import 'brace/mode/json';
import 'brace/theme/github';

import { NormalizedTable, Table } from '@/dataSources/types';
import { PGFunction } from '@/dataSources/services/postgresql/types';
import { generateTableDef } from '@/dataSources';
import { InputField } from '@/new-components/Form';
import { IconTooltip } from '@/new-components/Tooltip';
import { Collapse } from '@/new-components/Collapse';
import { getIngForm } from '../../../components/Services/Data/utils';

import JSONEditor from './JSONEditor';
import PermissionBuilder from '../PermissionBuilder/PermissionBuilder';

import { QueryType } from '../types';
import { DataLeaf } from '../../PermissionsTab/types/types';

const NoChecksLabel = () => (
  <span data-test="without-checks">
    Without any checks&nbsp;
    {/* {filterQueries['{}'] && (
              <i className={styles.add_mar_left_small}>
                (Same as <b>{filterQueries['{}'].join(', ')}</b>)
              </i>
            )} */}
  </span>
);

const CustomLabel = () => (
  <span data-test="custom-check" className="flex items-center">
    With custom check:
    <IconTooltip message="Create custom check using permissions builder" />
  </span>
);

export interface RowPermissionsProps {
  dataLeaf: DataLeaf;
  queryType: QueryType;
  subQueryType?: string;

  allRowChecks: Array<{ queryType: QueryType; value: string }>;
  allSchemas?: NormalizedTable[];
  allFunctions?: PGFunction[];
}

enum SelectedSection {
  NoChecks = 'no_checks',
  Custom = 'custom',
  NoneSelected = 'none',
}

const getRowPermission = (queryType: QueryType, subQueryType?: string) => {
  if (queryType === 'insert') {
    return 'check';
  }

  if (queryType === 'update') {
    if (subQueryType === 'pre') {
      return 'check';
    }

    return 'filter';
  }

  return 'filter';
};

const getRowPermissionCheckType = (
  queryType: QueryType,
  subQueryType?: string
) => {
  if (queryType === 'insert') {
    return 'checkType';
  }

  if (queryType === 'update') {
    if (subQueryType === 'pre') {
      return 'checkType';
    }

    return 'filterType';
  }

  return 'filterType';
};

export const RowPermissionsSection: React.FC<RowPermissionsProps> = ({
  queryType,
  subQueryType,
  dataLeaf,
  allRowChecks,
  allSchemas,
  allFunctions,
}) => {
  const { control, register, watch, setValue } = useFormContext();
  // determines whether the inputs should be pointed at `check` or `filter`
  const rowPermissions = getRowPermission(queryType, subQueryType);
  // determines whether the check type should be pointer at `checkType` or `filterType`
  const rowPermissionsCheckType = getRowPermissionCheckType(
    queryType,
    subQueryType
  );

  // if the query type is update and pre checks are not set, disable post checks
  const disabled =
    queryType === 'update' && subQueryType === 'post' && !watch('check');

  const schemaList = React.useMemo(
    () => allSchemas?.map(({ table_schema }) => table_schema),
    [allSchemas]
  );

  const selectedSection = watch(rowPermissionsCheckType);

  return (
    <fieldset className="grid gap-2">
      <div>
        <label className="flex items-center gap-2">
          <input
            id={SelectedSection.NoChecks}
            type="radio"
            value={SelectedSection.NoChecks}
            disabled={disabled}
            onClick={() => {
              setValue(rowPermissionsCheckType, SelectedSection.NoChecks);
              setValue(rowPermissions, '{}');
            }}
            {...register(rowPermissionsCheckType)}
          />
          <NoChecksLabel />
        </label>

        {selectedSection === SelectedSection.NoChecks && (
          <div className="pt-4">
            <JSONEditor
              data="{}"
              onChange={() =>
                setValue(rowPermissionsCheckType, SelectedSection.Custom)
              }
              initData="{}"
            />
          </div>
        )}
      </div>

      {allRowChecks?.map(({ queryType: query, value }) => (
        <div key={query}>
          <label className="flex items-center gap-2">
            <input
              id={`custom_${query}`}
              type="radio"
              value={query}
              disabled={disabled}
              onClick={() => {
                setValue(rowPermissionsCheckType, query);
                setValue(rowPermissions, value);
              }}
              {...register(rowPermissionsCheckType)}
            />
            <span data-test="mutual-check">
              With same custom check as&nbsp;<strong>{query}</strong>
            </span>
          </label>

          {selectedSection === query && (
            <div className="pt-4">
              <JSONEditor
                data={value}
                onChange={output => {
                  setValue(rowPermissionsCheckType, SelectedSection.Custom);
                  setValue(rowPermissions, output);
                }}
                initData=""
              />
            </div>
          )}
        </div>
      ))}

      <div>
        <label className="flex items-center gap-2">
          <input
            id={SelectedSection.Custom}
            type="radio"
            value={SelectedSection.Custom}
            disabled={disabled}
            {...register(rowPermissionsCheckType)}
          />
          <CustomLabel />
        </label>

        {selectedSection === SelectedSection.Custom && (
          <div className="pt-4">
            <Controller
              control={control}
              name={rowPermissions}
              render={({ field: { onChange, value } }) => (
                <>
                  <JSONEditor
                    data={value || '{}'}
                    onChange={output => {
                      onChange(output);
                    }}
                    initData="{}"
                  />

                  {allSchemas && allFunctions && schemaList && (
                    <PermissionBuilder
                      dispatchFuncSetFilter={output => {
                        onChange(output);
                      }}
                      loadSchemasFunc={() => {}}
                      tableDef={generateTableDef(
                        dataLeaf.leaf?.name || '',
                        dataLeaf.name
                      )}
                      allTableSchemas={allSchemas as Table[]}
                      allFunctions={allFunctions}
                      schemaList={schemaList}
                      filter={value || '{}'}
                      dispatch={() => console.log('output')}
                    />
                  )}
                </>
              )}
            />
          </div>
        )}
      </div>

      {queryType === 'select' && (
        <div className="w-40">
          <InputField label="Limit number of rows" name="rowCount" />
        </div>
      )}
    </fieldset>
  );
};

export interface RowPermissionsWrapperProps {
  queryType: QueryType;
  roleName: string;
  defaultOpen?: boolean;
}

const getStatus = (rowPermissions: string) => {
  if (!rowPermissions) {
    return 'No access';
  }

  if (rowPermissions === '{}') {
    return 'Without any checks';
  }

  return 'With custom checks';
};

export const RowPermissionsSectionWrapper: React.FC<RowPermissionsWrapperProps> =
  ({ children, queryType, roleName, defaultOpen }) => {
    const { watch } = useFormContext();

    const rowPermissions = watch('rowPermissions');
    const status = React.useMemo(
      () => getStatus(rowPermissions),
      [rowPermissions]
    );

    return (
      <Collapse
        title={`Row ${queryType} permissions`}
        tooltip={`Set permission rule for ${getIngForm(queryType)} rows`}
        status={status}
        defaultOpen={defaultOpen}
        data-test="toggle-row-permission"
      >
        <Collapse.Content>
          <div className="mb-2">
            <p>
              Allow role <strong>{roleName}</strong> to {queryType}&nbsp;
              <strong>rows</strong>:
            </p>
          </div>
          {children}
        </Collapse.Content>
      </Collapse>
    );
  };

export default RowPermissionsSection;
