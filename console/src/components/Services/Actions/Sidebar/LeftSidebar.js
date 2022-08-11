import React, { useMemo } from 'react';
import { FaBook, FaEdit, FaWrench } from 'react-icons/fa';
import { Link } from 'react-router';

import LeftSubSidebar from '../../../Common/Layout/LeftSubSidebar/LeftSubSidebar';
import styles from '../../../Common/Layout/LeftSubSidebar/LeftSubSidebar.module.scss';
import { inputStyles } from '../constants';

const LeftSidebar = ({
  appPrefix,
  common: { currentAction },
  actions,
  readOnlyMode,
}) => {
  const [searchText, setSearchText] = React.useState('');

  const handleSearch = e => setSearchText(e.target.value);

  const getSearchInput = () => {
    return (
      <input
        type="text"
        onChange={handleSearch}
        className={inputStyles}
        placeholder="search actions"
        data-test="search-actions"
      />
    );
  };

  const actionsList = useMemo(() => {
    if (!searchText) return actions;

    return actions.reduce((acc, action) => {
      const idx = action.name.search(RegExp(searchText, 'i'));
      if (idx === 0) return [action, ...acc];
      if (idx > 0) return [...acc, action];
      return acc;
    }, []);
  }, [searchText, actions]);

  const getActionIcon = action => {
    switch (action.definition.type) {
      case 'mutation':
        return <FaEdit className={styles.tableIcon} aria-hidden="true" />;
      case 'query':
        return <FaBook className={styles.tableIcon} aria-hidden="true" />;
      default:
        return <FaWrench className={styles.tableIcon} aria-hidden="true" />;
    }
  };

  const getChildList = () => {
    let childList;
    if (actionsList.length === 0) {
      childList = (
        <li
          data-test="actions-sidebar-no-actions"
          className="italic font-normal pb-sm pt-xs text-gray-500"
        >
          <i>No actions available</i>
        </li>
      );
    } else {
      childList = actionsList.map((a, i) => {
        let activeTableClass = '';
        if (a.name === currentAction) {
          activeTableClass = '!text-yellow-500';
        }

        const actionIcon = getActionIcon(a);

        return (
          <li key={i} data-test={`action-sidebar-links-${i + 1}`}>
            <Link
              className={activeTableClass}
              to={appPrefix + '/manage/' + a.name + '/modify'}
              data-test={a.name}
            >
              {actionIcon}
              {a.name}
            </Link>
          </li>
        );
      });
    }

    return childList;
  };

  return (
    <LeftSubSidebar
      showAddBtn={!readOnlyMode}
      searchInput={getSearchInput()}
      heading={`Actions (${actionsList.length})`}
      addLink={`${appPrefix}/manage/add`}
      addLabel={'Create'}
      addTestString={'actions-sidebar-add-table'}
      childListTestString={'actions-table-links'}
    >
      {getChildList()}
    </LeftSubSidebar>
  );
};

export default LeftSidebar;
