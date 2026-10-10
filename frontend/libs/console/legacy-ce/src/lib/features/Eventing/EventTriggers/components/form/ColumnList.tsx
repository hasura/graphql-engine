import React from 'react';
import {
  Button,
  CheckboxGroup,
  FieldLabel,
  IndicatorCard,
  Text,
} from '@hasura/shared/ui';
import { ColumnSelectionRadioButton } from './ColumnSelectionRadioButton';
import type { Table } from '@hasura/shared/types';
import { TableColumn } from '@hasura/metadata/data-source';
import { Em, Flex, Skeleton } from '@radix-ui/themes';
import { advancedOperationDescription } from '../../constants';

type ColumnListProps = {
  columns: TableColumn[] | undefined;
  hasUpdateOperation: boolean;
  selectedColumns: string[];
  areColumnsFetching: boolean;
  columnsFetchingError: unknown;
  table: Table | null;
  isAllColumnChecked: boolean;
  readOnlyMode: boolean;
  setOperationsColumns: (oc: string[]) => void;
  toggleAllColumnChecked: (value: boolean) => void;
};

const ColumnList: React.FC<ColumnListProps> = ({
  columns,
  columnsFetchingError,
  areColumnsFetching,
  table,
  isAllColumnChecked,
  readOnlyMode,
  setOperationsColumns,
  selectedColumns,
  hasUpdateOperation,
  toggleAllColumnChecked,
}: ColumnListProps) => {
  const renderColumnList = () => {
    if (!hasUpdateOperation) {
      return (
        <Text size="1">
          <Em>Applicable only if update operation is selected.</Em>
        </Text>
      );
    }

    if (!table) {
      return (
        <Text>
          <Em>Select a table first to get column list</Em>
        </Text>
      );
    }

    if (!columns?.length) {
      return (
        <IndicatorCard showIcon status="info">
          This table does have any column.
        </IndicatorCard>
      );
    }

    if (!areColumnsFetching && columnsFetchingError) {
      return (
        <IndicatorCard showIcon status="negative">
          Error happened when fetching table columns. Please try again.
        </IndicatorCard>
      );
    }

    const hasSelectedColumns = Boolean(selectedColumns?.length);

    const handleToggleAllColumns = () => {
      if (selectedColumns?.length) {
        setOperationsColumns([]);
        return;
      }

      const cols = columns.map((col) => col.name) ?? [];
      setOperationsColumns(cols);
    };

    return (
      <>
        <Skeleton loading={areColumnsFetching}>
          <ColumnSelectionRadioButton
            isAllColumnChecked={isAllColumnChecked}
            handleColumnRadioButton={toggleAllColumnChecked}
            readOnly={readOnlyMode}
          />
        </Skeleton>
        <div className="my-4">
          <Skeleton loading={areColumnsFetching}>
            {!isAllColumnChecked ? (
              <>
                <Flex gap="2" align="center" className="mb-4">
                  <Text>List of columns to select:</Text>
                  <Button
                    className="ml-2"
                    size="sm"
                    mode="default"
                    onClick={() => handleToggleAllColumns()}
                    disabled={readOnlyMode}
                  >
                    {hasSelectedColumns ? 'Unselect All' : 'Select All'}
                  </Button>
                </Flex>
                <CheckboxGroup
                  orientation="horizontal"
                  onChange={setOperationsColumns}
                  disabled={readOnlyMode}
                  value={selectedColumns}
                  options={
                    columns?.map((col) => ({
                      label: col.name,
                      value: col.name,
                    })) ?? []
                  }
                />
              </>
            ) : null}
          </Skeleton>
        </div>
      </>
    );
  };

  return (
    <div>
      <FieldLabel
        label="Listen columns for update"
        tooltip={advancedOperationDescription}
      />
      <div className="mt-4">{renderColumnList()}</div>
    </div>
  );
};

export default ColumnList;
