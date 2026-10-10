import React from 'react';
import { Link } from 'react-router';
import { Text, Tooltip } from '@hasura/shared/ui';
import { isBlockActive } from '../Main.utils';
import clsx from 'clsx';
import { Flex } from '@radix-ui/themes';
import { IconType } from 'react-icons';

export const linkStyle =
  'flex items-stretch gap-2 text-white font-bold rounded py-2 px-3 hover:!text-white active:text-primary focus:text-primary bg-transparent hover:bg-slate-900 no-underline! cursor-pointer';
export const activeLinkStyle =
  'text-primary hover:text-primary! focus:text-primary! visited:text-primary! bg-slate-900!';
export const itemContainerStyle = 'h-full flex items-center ml-xs';

interface NavItemProps {
  title: string;
  icon: IconType;
  tooltipText: string;
  path: string;
  appPrefix?: string;
  pathname: string;
  isDefault?: boolean;
}

const HeaderNavItem: React.FC<NavItemProps> = ({
  title,
  icon: Icon,
  tooltipText,
  path,
  appPrefix = '',
  pathname,
  isDefault = false,
}) => {
  const isCurrentBlockActive = isBlockActive({
    blockPath: path,
    isDefaultBlock: isDefault,
    pathname,
  });

  return (
    <Tooltip className="h-full" side="bottom" content={tooltipText}>
      <Link
        className={clsx(linkStyle, isCurrentBlockActive && activeLinkStyle)}
        to={appPrefix + path}
        data-test={`${title.toLowerCase()}-tab-link`}
      >
        <Flex gap="2" align="center">
          <Icon className="w-3 h-3" />
          <Text size="1" className="uppercase text-left">
            {title}
          </Text>
        </Flex>
      </Link>
    </Tooltip>
  );
};

export default HeaderNavItem;
