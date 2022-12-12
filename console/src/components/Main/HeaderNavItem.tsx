import React from 'react';
import { Link } from 'react-router';
import { getPathRoot } from '@/components/Common/utils/urlUtils';
import { Tooltip } from '@/new-components/Tooltip';
import { IconType } from 'react-icons';
import styles from './Main.module.scss';
import { isBlockActive } from './Main.utils';

interface NavItemProps {
  title: string;
  icon: IconType;
  tooltipText: string;
  path: string;
  appPrefix: string;
  pathname: string;
  isDefault?: boolean;
}

const HeaderNavItem: React.VFC<NavItemProps> = ({
  title,
  icon,
  tooltipText,
  path,
  appPrefix,
  pathname,
  isDefault = false,
}) => {
  const block = getPathRoot(path);

  const isCurrentBlockActive = isBlockActive({
    blockPath: path,
    isDefaultBlock: isDefault,
    pathname,
  });

  const className = isCurrentBlockActive ? styles.navSideBarActive : '';

  return (
    <li>
      <Tooltip side="bottom" tooltipContentChildren={tooltipText}>
        <Link
          className={className}
          to={appPrefix + path}
          data-test={`${title.toLowerCase()}-tab-link`}
        >
          <div className={styles.iconCenter} data-test={block}>
            {React.createElement(icon, {
              'aria-hidden': true,
            })}
          </div>
          <p className="uppercase">{title}</p>
        </Link>
      </Tooltip>
    </li>
  );
};

export default HeaderNavItem;
