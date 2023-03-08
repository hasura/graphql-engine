import React from 'react';
import { Link } from 'react-router';
import clsx from 'clsx';

import { Tooltip } from '@hasura/console-legacy-ce';

export const linkStyle =
  'flex items-stretch gap-2 text-white font-bold rounded py-2 px-3 hover:!text-white active:text-primary focus:text-primary bg-transparent hover:bg-slate-900 !no-underline cursor-pointer';
export const activeLinkStyle =
  'text-primary hover:!text-primary focus:!text-primary visited:!text-primary bg-slate-900';
export const itemContainerStyle = 'h-full flex items-center ml-xs';

const HeaderNavItem = ({
  title,
  icon,
  tooltipText,
  itemPath,
  linkPath,
  appPrefix,
  currentActiveBlock,
  isDefault = false,
}) => {
  const isActive =
    currentActiveBlock === itemPath || (isDefault && currentActiveBlock === '');

  return (
    <li>
      <Tooltip
        id={tooltipText}
        side="bottom"
        tooltipContentChildren={tooltipText}
        className="h-full"
      >
        <Link
          className={clsx(linkStyle, isActive && activeLinkStyle)}
          data-test={`${title.toLowerCase()}-tab-link`}
          to={appPrefix + linkPath}
        >
          <span className="text-sm self-baseline" data-test={itemPath}>
            {icon}
          </span>
          <span className="uppercase text-left">{title}</span>
        </Link>
      </Tooltip>
    </li>
  );
};

export default HeaderNavItem;
