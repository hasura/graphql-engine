import React from 'react';
import { Link } from 'react-router';
import { getPathRoot } from '../Common/utils/urlUtils';
import { Tooltip } from '../../new-components/Tooltip';
import { IconType } from 'react-icons';
import { isBlockActive } from './Main.utils';
import clsx from 'clsx';

export const linkStyle =
  'flex items-stretch gap-2 text-white font-bold rounded py-2 px-3 hover:!text-white active:text-primary focus:text-primary bg-transparent hover:bg-slate-900 !no-underline cursor-pointer';
export const activeLinkStyle =
  'text-primary hover:!text-primary focus:!text-primary visited:!text-primary bg-slate-900';
export const itemContainerStyle = 'h-full flex items-center ml-xs';

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

  return (
    <li>
      <Tooltip
        className="h-full"
        side="bottom"
        tooltipContentChildren={tooltipText}
      >
        <Link
          className={clsx(linkStyle, isCurrentBlockActive && activeLinkStyle)}
          to={appPrefix + path}
          data-test={`${title.toLowerCase()}-tab-link`}
        >
          <span className="text-sm self-baseline" data-test={block}>
            {icon}
          </span>
          <span className="uppercase text-left">{title}</span>
        </Link>
      </Tooltip>
    </li>
  );
};

export default HeaderNavItem;
