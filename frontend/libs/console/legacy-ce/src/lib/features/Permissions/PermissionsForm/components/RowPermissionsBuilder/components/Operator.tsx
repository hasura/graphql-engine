import isEmpty from 'lodash/isEmpty';
import { useContext, useMemo } from 'react';
import { GroupBase } from 'react-select';
import { ReactSelect, ReactSelectOptionType } from '@hasura/shared/ui';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { tableContext } from './TableProvider';
import { PermissionType } from './types';
import { logicalModelContext } from './RootLogicalModelProvider';
import { useForbiddenFeatures } from './ForbiddenFeaturesProvider';
import { rootTableContext } from './RootTableProvider';
import { areTablesEqual } from '@hasura/metadata/helpers';

type OperatorOption = ReactSelectOptionType<string> & {
  type: PermissionType;
};

export const Operator = ({
  operator,
  path,
  v,
}: {
  operator: string;
  path: string[];
  v: any;
}) => {
  const { operators, setKey, loadRelationships, isLoading } = useContext(
    rowPermissionsContext,
  );
  const { tables } = useContext(rootTableContext);
  const { columns, table, relationships, computedFields } =
    useContext(tableContext);
  const { rootLogicalModel } = useContext(logicalModelContext);

  const parent = path[path.length - 1];
  const operatorLevelId =
    path.length === 0
      ? 'root-operator'
      : `${path?.join('.')}-operator${operator ? `-root` : ''}`;
  const { hasFeature } = useForbiddenFeatures();

  const optionGroups = useMemo(() => {
    const groups: GroupBase<OperatorOption>[] = [];

    if (operators.boolean?.items.length) {
      groups.push({
        label: 'Bool operators',
        options: operators.boolean.items.map((item) => ({
          type: 'comparator',
          value: item.value,
          label: item.name,
        })),
      });
    }

    if (columns.length) {
      groups.push({
        label: 'Columns',
        options: columns.map((column) => ({
          type: column.dataType === 'object' ? 'object' : 'column',
          value: column.name,
          label: column.name,
        })),
      });
    }

    if (computedFields.length) {
      groups.push({
        label: 'Computed fields',
        options: computedFields.map((field) => ({
          type: 'computedField',
          value: field.name,
          label: field.name,
        })),
      });
    }

    if (rootLogicalModel?.fields.length) {
      groups.push({
        label: 'Columns',
        options: rootLogicalModel.fields.map((field) => ({
          type: 'column',
          value: field.name,
          label: field.name,
        })),
      });
    }

    if (hasFeature('exists') && operators.exist?.items.length) {
      groups.push({
        label: 'Exist operators',
        options: operators.exist.items.map((item) => ({
          type: 'exist',
          value: item.value,
          label: item.name,
        })),
      });
    }

    if (relationships.length) {
      groups.push({
        label: 'Relationships',
        options: relationships.map((item) => ({
          type: 'relationship',
          value: item.name,
          label: item.name,
        })),
      });
    }

    return groups;
  }, [
    operators.boolean,
    operators.exist,
    columns,
    computedFields,
    rootLogicalModel,
    relationships,
    hasFeature,
  ]);

  const selectedOption = useMemo(
    () =>
      optionGroups
        .flatMap((group) => group.options)
        .find((option) => option.value === operator) ?? null,
    [optionGroups, operator],
  );

  return (
    <div className="max-w-80">
      <ReactSelect<OperatorOption>
        inputId={`${operatorLevelId}-select-value`}
        aria-label={operatorLevelId}
        data-testid={operatorLevelId}
        isSearchable
        isDisabled={isLoading || (parent === '_where' && isEmpty(table))}
        value={selectedOption}
        options={optionGroups}
        filterOption={(option, inputValue) => {
          if (!inputValue) {
            return true;
          }

          return option.value.toLowerCase().includes(inputValue.toLowerCase());
        }}
        placeholder="-"
        onChange={(option) => {
          if (!option) {
            return;
          }

          const type = option.type;
          if (type === 'relationship') {
            const foundTable = tables.find((t) =>
              areTablesEqual(t.table, table),
            );
            if (foundTable) {
              loadRelationships?.(foundTable.relationships);
            }
          }
          setKey({ path, key: option?.value ?? '', type });
        }}
      />
    </div>
  );
};
