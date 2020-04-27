import React from 'react';
import styles from './LeftSubSidebar.scss';
import { Link } from 'react-router';

interface LeftSidebarItem {
  name: string;
}

interface LeftSidebarSectionProps extends React.ComponentProps<'div'> {
  items: LeftSidebarItem[];
  currentItem: LeftSidebarItem;
  getServiceEntityLink: (s: string) => string,
  service: string;
}

const getLeftSidebarSection = ({
  items = [],
  currentItem,
  service,
  getServiceEntityLink
}: LeftSidebarSectionProps) => {
  // TODO needs refactor to acoomodate other services
  const serviceIdentifiers: { [svc: string]: string } = {
    'event triggers': 'events',
    'scheduled triggers': 'scheduled',
  };

  const [searchText, setSearchText] = React.useState('');

  const getSearchInput = () => {
    const handleSearch = (e: React.BaseSyntheticEvent) =>
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

  // TODO test search
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
        <li
          className={styles.noChildren}
          data-test="sidebar-no-services"
        >
          <i>No {service} available</i>
        </li>
      );
    } else {
      childList = itemList.map((a, i) => {
        let activeTableClass = '';
        if (a.name === currentItem.name) {
          activeTableClass = styles.activeLink;
        }

        return (
          <li
            className={activeTableClass}
            key={i}
            data-test={`action-sidebar-links-${i + 1}`}
          >
            <Link
              to={getServiceEntityLink(a.name)}
              data-test={a.name}
            >
              <i
                className={styles.tableIcon + ' fa fa-wrench'}
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
  };
};

export default getLeftSidebarSection;
