import React from 'react';
import { Link } from 'react-router';
import styles from './LeftSubSidebar.scss';

interface LeftSidebarItem {
  name: string;
}

interface LeftSidebarSectionProps extends React.ComponentProps<'div'> {
  items: LeftSidebarItem[];
  currentItem?: LeftSidebarItem;
  getServiceEntityLink: (s: string) => string;
  service: string;
  sidebarIcon?: string;
}

const LeftSidebarSection = ({
  items = [],
  currentItem,
  service,
  sidebarIcon,
  getServiceEntityLink,
}: LeftSidebarSectionProps) => {
  // TODO needs refactor to accomodate other services

  const [searchText, setSearchText] = React.useState('');

  const getSearchInput = () => {
    const handleSearch = (e: React.ChangeEvent<HTMLInputElement>) =>
      setSearchText(e.target.value);
    return (
      <input
        type="text"
        onChange={handleSearch}
        className="form-control"
        placeholder={`search ${service}`}
        data-test={`search-${service}`}
      />
    );
  };

  let itemList: LeftSidebarItem[] = [];
  if (searchText) {
    const secondaryResults: LeftSidebarItem[] = [];
    items.forEach(a => {
      if (a.name.startsWith(searchText)) {
        itemList.push(a);
      } else if (a.name.includes(searchText)) {
        secondaryResults.push(a);
      }
    });
    itemList = [...itemList, ...secondaryResults];
  } else {
    itemList = [...items];
  }

  const getChildList = () => {
    let childList;
    if (itemList.length === 0) {
      childList = (
        <li className={styles.noChildren} data-test="sidebar-no-services">
          <i>No {service} available</i>
        </li>
      );
    } else {
      childList = itemList.map(a => {
        let activeTableClass = '';
        if (currentItem && currentItem.name === a.name) {
          activeTableClass = styles.activeLink;
        }

        return (
          <li
            className={activeTableClass}
            key={a.name}
            data-test={`action-sidebar-links-${a.name}`}
          >
            <Link to={getServiceEntityLink(a.name)} data-test={a.name}>
              <i
                className={`${styles.tableIcon} fa ${
                  sidebarIcon || 'fa-wrench'
                }`}
                aria-hidden="true"
              />
              {a.name}
            </Link>
          </li>
        );
      });
    }

    return childList;
  };

  return {
    getChildList,
    getSearchInput,
    count: itemList.length,
    items: itemList,
  };
};

export default LeftSidebarSection;
