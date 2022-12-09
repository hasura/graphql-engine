import { getTableDisplayName } from '@/features/DatabaseRelationships';
import { Table } from '@/features/hasura-metadata-types';
import produce from 'immer';
import React, { useState } from 'react';
import {
  DataGrid,
  DataGridOptions,
  DataGridProps,
} from './components/DataGrid/DataGrid';
import { TableTabView } from './components/DataGrid/parts/TableTabView';

interface BrowseRowsProps {
  dataSourceName: string;
  table: Table;
  options?: DataGridOptions;
}

type TabState = { name: string; details: DataGridProps; parentValue: string };
type OpenNewRelationshipTabProps = {
  data: {
    table: Table;
    dataSourceName: string;
    options?: DataGridOptions;
    relationshipName: string;
    parentOptions?: DataGridOptions;
  };
  openTab: { name: string; details: DataGridProps; parentValue: string };
};

const onTabClose = (
  openTabs: TabState[],
  setOpenTabs: React.Dispatch<React.SetStateAction<TabState[]>>,
  tabName: string,
  originalTableOptions: DataGridOptions | undefined,
  activeTab: string,
  setActiveTab: React.Dispatch<React.SetStateAction<string>>
) => {
  /**
   * Get the tab object from the state
   */
  const tabToRemove = openTabs.find(t => t.name === tabName);
  /**
   * get the name of the relationship that closing tab represents
   */
  const relationshipNameToRemove = tabName.split('.').pop();

  const updatedOpenTabs =
    /**
     * remove the closed tab from the list
     */
    openTabs
      .filter(t => t.name !== tabName)
      /**
       * update its parent tab -> remove the relationshipNameToRemove from the activeRelationships list
       */
      .map((t, index) => {
        if (t.name !== tabToRemove?.parentValue) return t;

        const updatedActiveRelationships =
          t.details.activeRelationships?.filter(
            r => r !== relationshipNameToRemove
          );

        const isOriginalTable = index === 0;

        /**
         * if its the original tab and there are no more relationships active, then reset everything back
         */
        if (isOriginalTable && !updatedActiveRelationships?.length) {
          return produce(t, draft => {
            draft.details.activeRelationships = [];
            draft.details.options = originalTableOptions;
          });
        }

        return produce(t, draft => {
          draft.details.activeRelationships =
            draft.details.activeRelationships?.filter(
              r => r !== relationshipNameToRemove
            );
        });
      });

  setOpenTabs([...updatedOpenTabs]);

  /**
   * Navigate to the correct tab if the closed tab was the active one.
   */
  if (activeTab === tabName) {
    const previousTab = openTabs.findIndex(t => t.name === tabName);
    const previousTabName = openTabs[previousTab - 1].name;
    setActiveTab(previousTabName);
  }
};

export const BrowseRows = (props: BrowseRowsProps) => {
  const { dataSourceName, table, options } = props;

  const defaultTabState = {
    name: getTableDisplayName(table),
    details: { dataSourceName, table, options },
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
      ...openTabs.map(_openTab => {
        if (_openTab.name !== activeTab) return _openTab;
        return produce(_openTab, draft => {
          draft.details.options = parentOptions;
          draft.details.activeRelationships = [
            ...(_openTab.details.activeRelationships ?? []),
            relationshipName,
          ];
        });
      }),
      {
        name: `${openTab.name}.${relationshipName}`,
        details,
        parentValue: openTab.name,
      },
    ]);
    setActiveTab(`${openTab.name}.${relationshipName}`);
  };

  return (
    <div>
      <TableTabView
        items={openTabs.map((openTab, index) => ({
          value: openTab.name,
          label: openTab.name,
          parentValue: openTab.parentValue,
          content: (
            <DataGrid
              key={JSON.stringify(openTab)}
              table={openTab.details.table}
              dataSourceName={openTab.details.dataSourceName}
              options={openTab.details.options}
              activeRelationships={openTab.details.activeRelationships}
              onRelationshipOpen={data =>
                openNewRelationshipTab({ data, openTab })
              }
              onRelationshipClose={relationshipName =>
                setActiveTab(`${openTab.name}.${relationshipName}`)
              }
              disableRunQuery={disableRunQuery}
              updateOptions={_options => {
                if (index === 0) {
                  // Save a copy of the parent filters before opening
                  setOriginalTableOptions(_options);
                }

                setOpenTabs(_openTabs =>
                  produce(_openTabs, draft => {
                    draft[index].details.options = _options;
                  })
                );
              }}
            />
          ),
        }))}
        activeTab={activeTab}
        onTabClick={value => {
          setActiveTab(value);
        }}
        onTabClose={tabName =>
          onTabClose(
            openTabs,
            setOpenTabs,
            tabName,
            originalTableOptions,
            activeTab,
            setActiveTab
          )
        }
        onCloseAll={() => {
          setOpenTabs(
            produce(openTabs, draft => {
              draft.splice(1);
              draft[0] = defaultTabState;
              draft[0].details.options = originalTableOptions;
            })
          );
          setActiveTab(defaultActiveTab);
        }}
      />
    </div>
  );
};
