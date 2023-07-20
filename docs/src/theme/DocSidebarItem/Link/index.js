import React from 'react';
import clsx from 'clsx';
import { ThemeClassNames } from '@docusaurus/theme-common';
import { isActiveSidebarItem } from '@docusaurus/theme-common/internal';
import Link from '@docusaurus/Link';
import isInternalUrl from '@docusaurus/isInternalUrl';
import styles from './styles.module.scss';
import { addIconsToLabel } from '../utils';

export default function DocSidebarItemLink({
  item,
  onItemClick,
  activePath,
  level,
  index,
  ...props
}) {
  const { href, label, className, autoAddBaseUrl } = item;
  const isActive = isActiveSidebarItem(item, activePath);
  const isInternalLink = isInternalUrl(href);

  const labelWithIcons = addIconsToLabel(label, className);

  return (
    <li
      className={clsx(
        ThemeClassNames.docs.docSidebarItemLink,
        ThemeClassNames.docs.docSidebarItemLinkLevel(level),
        'menu__list-item',
        className,
        styles[`sidebar_link_wrapper`]
      )}
      key={label}
    >
      {className !== 'sidebar_heading' ? (
        <Link
          className={clsx(
            'menu__link',
            !isInternalLink && styles.menuExternalLink,
            {
              'menu__link--active': isActive,
            }
          )}
          autoAddBaseUrl={autoAddBaseUrl}
          aria-current={isActive ? 'page' : undefined}
          to={href}
          {...(isInternalLink && {
            onClick: onItemClick ? () => onItemClick(item) : undefined,
          })}
          {...props}
        >
          {labelWithIcons}
        </Link>
      ) : (
        <>{labelWithIcons}</>
      )}
    </li>
  );
}
