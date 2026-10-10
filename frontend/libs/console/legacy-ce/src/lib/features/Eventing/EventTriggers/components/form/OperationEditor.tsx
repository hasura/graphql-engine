import React from 'react';
import { Heading } from '@radix-ui/themes';
import { EventTriggerOperation } from '../../types';
import {
  parseEventTriggerOperations,
  getETOperationColumns,
} from '../../utils';
import { Operations } from './Operations';
import { EventTrigger, Table } from '@hasura/shared/types';
import { TableColumn } from '@hasura/metadata/data-source';
import ColumnList from './ColumnList';
import {
  ExpandableEditor,
  ExpandableEditorFunction,
  Text,
} from '@hasura/shared/ui';

type OperationEditorProps = {
  readOnlyMode: boolean;
  currentTrigger: EventTrigger;
  table: Table;
  columns: TableColumn[];
  operations: EventTriggerOperation[];
  setOperations: (o: EventTriggerOperation[]) => void;
  operationColumns: string[];
  setOperationColumns: (operationColumns: string[]) => void;
  save: ExpandableEditorFunction;
  toggleAllColumnChecked: (value: boolean) => void;
  isAllColumnChecked: boolean;
  areColumnsFetching: boolean;
  columnsFetchingError: unknown;
};

export const OperationEditor: React.FC<OperationEditorProps> = ({
  table,
  columns,
  save,
  readOnlyMode,
  currentTrigger,
  operations,
  operationColumns,
  setOperations,
  setOperationColumns,
  isAllColumnChecked,
  areColumnsFetching,
  columnsFetchingError,
  toggleAllColumnChecked,
}) => {
  const etDef = currentTrigger.definition;
  const existingOps = parseEventTriggerOperations(etDef);
  const existingOpColumns = getETOperationColumns(
    etDef.update ? etDef.update.columns : [],
    columns,
  );

  const reset = () => {
    setOperations(existingOps);
    setOperationColumns(existingOpColumns);
  };

  const renderEditor = (
    ops: EventTriggerOperation[],
    opCols: string[],
    readOnly: boolean,
  ) => {
    return (
      <div className="pt-2">
        <Text weight="medium">Trigger Method</Text>
        <div className="my-4 w-full">
          <Operations
            selectedOperations={ops}
            setOperations={setOperations}
            readOnly={readOnly}
            table={table}
          />
        </div>

        <ColumnList
          hasUpdateOperation={operations.includes('UPDATE')}
          areColumnsFetching={areColumnsFetching}
          columns={columns}
          columnsFetchingError={columnsFetchingError}
          selectedColumns={opCols}
          table={table}
          isAllColumnChecked={isAllColumnChecked}
          readOnlyMode={readOnlyMode}
          setOperationsColumns={setOperationColumns}
          toggleAllColumnChecked={toggleAllColumnChecked}
        />
      </div>
    );
  };

  const collapsed = () => renderEditor(existingOps, existingOpColumns, true);

  const expanded = () => renderEditor(operations, operationColumns, false);

  return (
    <div className="w-full">
      <div className="mb-2">
        <Heading size="3">Trigger Operations</Heading>
      </div>
      <ExpandableEditor
        editorCollapsed={collapsed}
        editorExpanded={expanded}
        property="ops"
        service="modify-trigger"
        saveFunc={save}
        expandCallback={reset}
        dataTest="edit-operations"
      />
    </div>
  );
};
