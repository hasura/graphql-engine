import React from 'react';
import { Link as RouterLink } from 'react-router';

import LeftSubSidebar from '../../../Common/Layout/LeftSubSidebar/LeftSubSidebar';
import styles from '../../../Common/Layout/LeftSubSidebar/LeftSubSidebar.scss';
import { Icon } from '../../../UIKit/atoms';

const LeftSidebar = ({ appPrefix, common: { actions, currentAction } }) => {
  const [searchText, setSearchText] = React.useState('');

  const handleSearch = e => setSearchText(e.target.value);

  const getSearchInput = () => {
    return (
      <input
        type="text"
        onChange={handleSearch}
        className="form-control"
        placeholder="search actions"
        data-test="search-actions"
      />
    );
  };

  // TODO test search
  let actionsList = [];
  if (searchText) {
    const secondaryResults = [];
    actions.forEach(a => {
      if (a.action_name.startsWith(searchText)) {
        actionsList.push(a);
      } else if (a.action_name.includes(searchText)) {
        secondaryResults.push(a);
      }
    });
    actionsList = [...actionsList, ...secondaryResults];
  } else {
    actionsList = [...actions];
  }

  const getChildList = () => {
    let childList;
    if (actionsList.length === 0) {
      childList = (
        <li
          className={styles.noChildren}
          data-test="actions-sidebar-no-actions"
        >
          <i>No actions available</i>
        </li>
      );
    } else {
      childList = actionsList.map((a, i) => {
        let activeTableClass = '';
        if (a.action_name === currentAction) {
          activeTableClass = styles.activeLink;
        }

        return (
          <li
            className={activeTableClass}
            key={i}
            data-test={`action-sidebar-links-${i + 1}`}
          >
            <RouterLink
              to={appPrefix + '/manage/' + a.action_name + '/modify'}
              data-test={a.action_name}
            >
              <Icon type="wrench" mr="xs" size={12} />
              {a.action_name}
            </RouterLink>
          </li>
        );
      });
    }

    return childList;
  };

  return (
    <LeftSubSidebar
      showAddBtn
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
