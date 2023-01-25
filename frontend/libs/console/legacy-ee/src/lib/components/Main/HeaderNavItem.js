import styles from "./Main.module.scss";
import {Link} from "react-router";
import React from "react";

import {
  Tooltip,
} from '@hasura/console-oss';


const HeaderNavItem = ({
  title,
  icon,
  tooltipText,
  itemPath,
  linkPath,
  appPrefix,
  currentActiveBlock,
  isDefault = false
}) => {
  const className = currentActiveBlock === itemPath || (isDefault && currentActiveBlock === '') ? styles.navSideBarActive : '';

  return (
    <li>
      <Tooltip
        id={tooltipText}
        side="bottom"
        tooltipContentChildren={tooltipText}
      >
        <Link
          className={className}
          data-test={`${title.toLowerCase()}-tab-link`}
          to={appPrefix + linkPath}
        >
          <div className={styles.iconCenter} data-test={itemPath}>
            {icon}
          </div>
          <p className="uppercase">{title}</p>
        </Link>
      </Tooltip>
    </li>
  );
}

export default HeaderNavItem;