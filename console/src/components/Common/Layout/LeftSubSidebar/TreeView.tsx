import React, { useMemo, useState } from 'react';

import { Link } from 'react-router';

import styles from './LeftSubSidebar.scss';

type CollapsibleItemsProps = {
  source: string;
  currentItem?: { name: string; source: string };
  getServiceEntityLink: (v: string) => string;
  items: { name: string; source: string }[];
  icon: string;
};
const CollapsibleItems: React.FC<CollapsibleItemsProps> = ({
  currentItem,
  source,
  getServiceEntityLink,
  items,
  icon,
}) => {
  const [isOpen, setIsOpen] = useState(true);

  return (
    <div className={styles.padd_bottom_small}>
      <div
        onClick={() => setIsOpen(prev => !prev)}
        onKeyDown={() => setIsOpen(prev => !prev)}
        role="button"
        className={styles.padd_bottom_small}
      >
        <span className={`${styles.title} ${isOpen ? '' : styles.titleClosed}`}>
          <i className="fa fa-database" /> {source}
        </span>
      </div>
      {isOpen
        ? items.map(({ name }) => (
            <li
              className={
                currentItem && currentItem.name === name
                  ? styles.activeLink
                  : ''
              }
              key={name}
              data-test={`action-sidebar-links-${name}`}
            >
              <Link to={getServiceEntityLink(name)} data-test={name}>
                <i className={`fa ${icon || 'fa-wrench'}`} aria-hidden="true" />
                {name}
              </Link>
            </li>
          ))
        : null}
    </div>
  );
};

type TreeViewProps = {
  service: string;
  items: { name: string; source: string }[];
  currentItem?: { name: string; source: string };
  getServiceEntityLink: (name: string) => string;
  icon: string;
};
export const TreeView: React.FC<TreeViewProps> = ({
  items,
  service,
  ...rest
}) => {
  const itemsBySource = useMemo(() => {
    return items.reduce((acc, item) => {
      return {
        ...acc,
        [item.source]: acc[item.source] ? [...acc[item.source], item] : [item],
      };
    }, {} as Record<string, { name: string; source: string }[]>);
  }, [items]);

  if (items.length === 0) {
    return (
      <li className={styles.noChildren} data-test="sidebar-no-services">
        <i>No {service} available</i>
      </li>
    );
  }

  return (
    <div className={styles.treeNav}>
      {Object.keys(itemsBySource).map(source => (
        <CollapsibleItems
          source={source}
          items={itemsBySource[source]}
          {...rest}
        />
      ))}
    </div>
  );
};
