import { LogicalModel, Table } from '@hasura/shared/types';
import {
  Tables,
  Operators,
  Permissions,
  Comparators,
  Relationships,
} from './types';
import { RowPermissionsProvider } from './RowPermissionsProvider';
import { TypesProvider } from './TypesProvider';
import { TableProvider } from './TableProvider';
import { RootInput } from './RootInput';
import { JsonEditor } from './JsonEditor';
import { RootTableProvider } from './RootTableProvider';
import { RootLogicalModelProvider } from './RootLogicalModelProvider';
import {
  ForbiddenFeaturesProvider,
  Feature,
} from './ForbiddenFeaturesProvider';
import { LogicalModelWithSource } from '@hasura/metadata/helpers';
import { Flex, SegmentedControl } from '@radix-ui/themes';
import { useState } from 'react';

/** the permissions are edited either with the builder or as JSON */
type EditorMode = 'builder' | 'json';

const operators: Operators = {
  boolean: {
    label: 'Bool operators',
    items: [
      { name: '_and', value: '_and' },
      { name: '_not', value: '_not' },
      { name: '_or', value: '_or' },
    ],
  },
  exist: {
    label: 'Exist operators',
    items: [{ name: '_exists', value: '_exists' }],
  },
};

export const RowPermissionsInput = ({
  permissions,
  tables,
  table,
  source,
  logicalModel,
  logicalModels,
  onPermissionsChange,
  onLoadRelationships,
  comparators,
  forbidden,
  isLoading,
}: {
  permissions: Permissions;
  tables: Tables;
  table: Table | undefined;
  /** the source of the table, as sources can track tables with the same name */
  source?: string;
  logicalModels: LogicalModelWithSource[];
  logicalModel: LogicalModel['name'] | undefined;
  onPermissionsChange?: (permissions: Permissions) => void;
  onLoadRelationships?: (relationships: Relationships) => void;
  comparators: Comparators;
  forbidden?: Feature[];
  isLoading?: boolean;
}) => {
  const [editorMode, setEditorMode] = useState<EditorMode>('builder');

  return (
    <ForbiddenFeaturesProvider forbidden={forbidden}>
      <RootLogicalModelProvider
        logicalModel={logicalModel}
        logicalModels={logicalModels}
      >
        <RootTableProvider table={table} tables={tables} source={source}>
          <RowPermissionsProvider
            operators={operators}
            permissions={permissions}
            comparators={comparators}
            onPermissionsChange={onPermissionsChange}
            loadRelationships={onLoadRelationships}
            isLoading={isLoading}
          >
            <TypesProvider>
              <TableProvider table={table}>
                <Flex direction="column" gap="2" className="w-full">
                  <Flex justify="end">
                    <SegmentedControl.Root
                      size="1"
                      value={editorMode}
                      onValueChange={(value) =>
                        setEditorMode(value as EditorMode)
                      }
                      aria-label="Permissions editor"
                    >
                      <SegmentedControl.Item value="builder">
                        Builder
                      </SegmentedControl.Item>
                      <SegmentedControl.Item value="json">
                        JSON
                      </SegmentedControl.Item>
                    </SegmentedControl.Root>
                  </Flex>
                  {editorMode === 'json' ? <JsonEditor /> : <RootInput />}
                </Flex>
              </TableProvider>
            </TypesProvider>
          </RowPermissionsProvider>
        </RootTableProvider>
      </RootLogicalModelProvider>
    </ForbiddenFeaturesProvider>
  );
};
