import { Source, Table } from '@hasura/shared/types';
import { getTableDisplayName } from '@hasura/shared/utils';
import React, { useEffect, useState } from 'react';
import { setWhereAndSortToUrl } from './BrowseRows.utils';
import {
  DataGrid,
  DataGridOptions,
  DataGridProps,
  OnRelationshipOpenProps,
} from './components/DataGrid/DataGrid';
import { TableTabView } from './components/DataGrid/parts/TableTabView';

type BrowseRowsProps = {
  source: Source;
  table: Table;
  options?: DataGridOptions;
  onUpdateOptions: (options: DataGridOptions) => void;
};

type TabState = { name: string; details: DataGridProps; parentValue: string };
type OpenNewRelationshipTabProps = {
  data: OnRelationshipOpenProps;
  openTab: { name: string; details: DataGridProps; parentValue: string };
};

const onTabClose = (
  openTabs: TabState[],
  setOpenTabs: React.Dispatch<React.SetStateAction<TabState[]>>,
  tabName: string,
  originalTableOptions: DataGridOptions | undefined,
  activeTab: string,
  setActiveTab: React.Dispatch<React.SetStateAction<string>>,
) => {
  /**
   * Get the tab object from the state
   */
  const tabToRemove = openTabs.find((t) => t.name === tabName);
  /**
   * get the name of the relationship that closing tab represents
   */
  const relationshipNameToRemove = tabName.split('.').pop();

  const updatedOpenTabs =
    /**
     * remove the closed tab from the list
     */
    openTabs
      .filter((t) => t.name !== tabName)
      /**
       * update its parent tab -> remove the relationshipNameToRemove from the activeRelationships list
       */
      .map((t, index) => {
        if (t.name !== tabToRemove?.parentValue) return t;

        const updatedActiveRelationships =
          t.details.activeRelationships?.filter(
            (r) => r !== relationshipNameToRemove,
          );

        const isOriginalTable = index === 0;

        /**
         * if its the original tab and there are no more relationships active, then reset everything back
         */
        if (isOriginalTable && !updatedActiveRelationships?.length) {
          return {
            ...t,
            details: {
              ...t.details,
              activeRelationships: [],
              options: originalTableOptions,
            },
          };
        }

        return {
          ...t,
          details: {
            ...t.details,
            activeRelationships: t.details.activeRelationships?.filter(
              (r) => r !== relationshipNameToRemove,
            ),
          },
        };
      });

  setOpenTabs([...updatedOpenTabs]);

  /**
   * Navigate to the correct tab if the closed tab was the active one.
   */
  if (activeTab === tabName) {
    const previousTab = openTabs.findIndex((t) => t.name === tabName);
    const previousTabName = openTabs[previousTab - 1].name;
    setActiveTab(previousTabName);
  }
};

export const BrowseRows = ({
  source,
  table,
  options,
  onUpdateOptions,
}: BrowseRowsProps) => {
  const defaultTabState: TabState = {
    name: getTableDisplayName(table),
    details: {
      source,
      table,
      options,
    },
    parentValue: '',
  };
  const [originalTableOptions, setOriginalTableOptions] =
    useState<DataGridOptions>();

  /**
   * This maintains the state of all open tables. The first table will ALWAYS be the original table in question. The rest of them will
   * be its relationship views.
   */
  const [openTabs, setOpenTabs] = useState<TabState[]>([defaultTabState]);

  /**
   * State that contains the unique name of the active tab
   */
  const defaultActiveTab = getTableDisplayName(table);
  const [activeTab, setActiveTab] = useState(defaultActiveTab);

  useEffect(() => {
    setOriginalTableOptions(options);
    const currentTab = openTabs[0];

    setOpenTabs([
      {
        ...currentTab,
        details: {
          ...currentTab.details,
          options,
        },
      },
    ]);
  }, [options]);

  /**
   * when relationships are open, disable sorting and searching through all the views
   */
  const disableRunQuery = openTabs.length > 1;

  const openNewRelationshipTab = ({
    data,
    openTab,
  }: OpenNewRelationshipTabProps) => {
    const { relationshipName, parentOptions, ...details } = data;
    setOpenTabs([
      ...openTabs.map((_openTab) => {
        if (_openTab.name !== activeTab) return _openTab;

        return {
          ..._openTab,
          details: {
            ..._openTab.details,
            options: parentOptions,
            activeRelationships: [
              ...(_openTab.details.activeRelationships ?? []),
              relationshipName,
            ],
          },
        };
      }),
      {
        name: `${openTab.name}.${relationshipName}`,
        details: {
          ...details,
          source,
        },
        parentValue: openTab.name,
      },
    ]);
    setActiveTab(`${openTab.name}.${relationshipName}`);
  };

  const onUpdateOptionsGenerator =
    (index: number) =>
    (_options: DataGridOptions): void => {
      setWhereAndSortToUrl(_options);

      setOriginalTableOptions(_options);

      setOpenTabs((_openTabs) =>
        _openTabs.map((item, i) => {
          if (i !== index) {
            return item;
          }

          return {
            ...item,
            details: {
              ...item.details,
              options: _options,
            },
          };
        }),
      );

      onUpdateOptions(_options);
    };

  const tableTabItems = openTabs.map((openTab, index) => {
    const innerOnUpdateOptions = onUpdateOptionsGenerator(index);
    return {
      value: openTab.name,
      label: openTab.name,
      parentValue: openTab.parentValue,
      content: (
        <DataGrid
          key={JSON.stringify(openTab)}
          table={openTab.details.table}
          source={openTab.details.source}
          options={openTab.details.options}
          activeRelationships={openTab.details.activeRelationships}
          onRelationshipOpen={(data) =>
            openNewRelationshipTab({ data, openTab })
          }
          onRelationshipClose={(relationshipName) =>
            setActiveTab(`${openTab.name}.${relationshipName}`)
          }
          disableRunQuery={disableRunQuery}
          updateOptions={innerOnUpdateOptions}
        />
      ),
    };
  });

  return (
    <div>
      <TableTabView
        items={tableTabItems}
        activeTab={activeTab}
        onTabClick={(value) => {
          setActiveTab(value);
        }}
        onTabClose={(tabName) =>
          onTabClose(
            openTabs,
            setOpenTabs,
            tabName,
            originalTableOptions,
            activeTab,
            setActiveTab,
          )
        }
      />
    </div>
  );
};
