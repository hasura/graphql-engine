import {
  DropdownButton,
  Badge,
  LeftSubSidebar,
  DropdownMenu,
  Text,
  Input,
  LeftSidebarLeafNavItem,
} from '@hasura/shared/ui';
import { Analytics } from '@hasura/shared/analytics';
import React, { useMemo } from 'react';
import { FaBook, FaEdit, FaFileImport, FaWrench } from 'react-icons/fa';
import { useNavigate } from 'react-router';
import { useAppContext } from '@hasura/shared/context';
import type { Action } from '@hasura/shared/types';
import { Em, Flex } from '@radix-ui/themes';
import { dataRoutes } from '@hasura/shared/utils';

type Props = {
  currentAction: string | undefined;
  actions: Action[];
  allowOpenApiImport?: boolean;
};

const LeftSidebar = ({ currentAction, actions, allowOpenApiImport }: Props) => {
  const navigate = useNavigate();
  const { readOnlyMode } = useAppContext();
  const [searchText, setSearchText] = React.useState('');

  const handleSearch = (e) => setSearchText(e.target.value);

  const findIfSubStringExists = (originalString, subString) => {
    return originalString.toLowerCase().includes(subString.toLocaleLowerCase());
  };

  const actionsList = useMemo(() => {
    if (!searchText) return actions;

    return actions.reduce((acc, action) => {
      const idx = findIfSubStringExists(action.name, searchText);
      if (idx === 0) return [action, ...acc];
      if (idx > 0) return [...acc, action];
      return acc;
    }, [] as Action[]);
  }, [searchText, actions]);

  const getActionIcon = (action) => {
    switch (action.definition.type) {
      case 'mutation':
        return <FaEdit aria-hidden="true" />;
      case 'query':
        return <FaBook aria-hidden="true" />;
      default:
        return <FaWrench aria-hidden="true" />;
    }
  };

  const getChildList = () => {
    if (actionsList.length === 0) {
      return (
        <Text as="p" data-test="actions-sidebar-no-actions">
          <Em>No actions available</Em>
        </Text>
      );
    }

    return actionsList.map((a, i) => {
      const actionIcon = getActionIcon(a);

      return (
        <LeftSidebarLeafNavItem
          key={i}
          to={dataRoutes.manageAction(a.name, 'modify')}
          isActive={a.name === currentAction}
        >
          <Flex align="center" gap="1">
            {actionIcon}
            {a.name}
          </Flex>
        </LeftSidebarLeafNavItem>
      );
    });
  };

  return (
    <LeftSubSidebar
      showAddBtn={!readOnlyMode}
      searchInput={
        <Input
          type="text"
          onChange={handleSearch}
          placeholder="search actions"
          data-test="search-actions"
        />
      }
      heading={`Actions (${actionsList.length})`}
      addLink={dataRoutes.createAction}
      addLabel={'Create'}
      addTestString={'actions-sidebar-add-table'}
      childListTestString={'actions-table-links'}
      addBtn={
        allowOpenApiImport ? (
          <DropdownButton
            mode="default"
            size="1"
            items={[
              <Analytics
                key="action-tab-button-add-actions-sidebar-with-form"
                name="action-tab-button-add-actions-sidebar-with-form"
              >
                <DropdownMenu.Item
                  className="py-1 "
                  onClick={() => {
                    navigate(dataRoutes.createAction);
                  }}
                >
                  <FaEdit className="relative -top-px" /> New Action
                </DropdownMenu.Item>
              </Analytics>,
              <Analytics
                key="action-tab-button-add-actions-sidebar-openapi"
                name="action-tab-button-add-actions-sidebar-openapi"
              >
                <DropdownMenu.Item
                  className="py-1 "
                  onClick={() => {
                    navigate(dataRoutes.manageAction('add-oas'));
                  }}
                >
                  <FaFileImport className="relative left-[-2px] -top-px" />{' '}
                  Import OpenAPI{' '}
                  <Badge className="ml-1 font-xs" color="purple">
                    New
                  </Badge>
                </DropdownMenu.Item>
              </Analytics>,
            ]}
          >
            Create
          </DropdownButton>
        ) : undefined
      }
    >
      {getChildList()}
    </LeftSubSidebar>
  );
};

export default LeftSidebar;
