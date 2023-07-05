import React from 'react';
import clsx from 'clsx';
import { ThemeClassNames } from '@docusaurus/theme-common';
import { isActiveSidebarItem } from '@docusaurus/theme-common/internal';
import Link from '@docusaurus/Link';
import isInternalUrl from '@docusaurus/isInternalUrl';
import IconExternalLink from '@theme/Icon/ExternalLink';
import styles from './styles.module.scss';
import { useColorMode } from '@docusaurus/theme-common';
import EnterpriseLight from '@site/static/icons/enterprise-dark.svg';
import EnterpriseDark from '@site/static/icons/enterprise-light.svg';
import CloudLight from '@site/static/icons/cloud-dark.svg';
import CloudDark from '@site/static/icons/cloud-light.svg';
import BetaTag from '@site/src/components/BetaTag/BetaTag';
export default function DocSidebarItemLink({ item, onItemClick, activePath, level, index, ...props }) {
  const { href, label, className, autoAddBaseUrl } = item;
  const isActive = isActiveSidebarItem(item, activePath);
  const isInternalLink = isInternalUrl(href);
  const { colorMode } = useColorMode();

  // Conditional rendering for sidebar icons
  function addIcons(className) {
    switch (className) {
      case 'enterprise-icon':
        return colorMode === 'dark' ? <EnterpriseDark /> : <EnterpriseLight />;
      case 'cloud-icon':
        return colorMode === 'dark' ? <CloudDark /> : <CloudLight />;
      case 'enterprise-icon-and-beta':
        return (
          <div className={styles['sidebar_link_wrapper']}>
            {colorMode === 'dark' ? (
              <>
                <EnterpriseDark /> <BetaTag />
              </>
            ) : (
              <>
                <EnterpriseLight /> <BetaTag />
              </>
            )}
          </div>
        );
      case 'cloud-and-enterprise-icon':
        return (
          <div className={styles['cloud-ee-container']}>
            {colorMode === 'dark' ? (
              <>
                <CloudDark /> <EnterpriseDark />{' '}
              </>
            ) : (
              <>
                <CloudLight /> <EnterpriseLight />
              </>
            )}
          </div>
        );
      case 'beta-icon':
        return (
          <div className={styles['sidebar_link_wrapper']}>
            <BetaTag />
          </div>
        );
      default:
        return null;
    }
  }

  if (className != 'sidebar_heading') {
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
        <Link
          className={clsx('menu__link', !isInternalLink && styles.menuExternalLink, {
            'menu__link--active': isActive,
          })}
          autoAddBaseUrl={autoAddBaseUrl}
          aria-current={isActive ? 'page' : undefined}
          to={href}
          {...(isInternalLink && {
            onClick: onItemClick ? () => onItemClick(item) : undefined,
          })}
          {...props}
        >
          {label}
          {addIcons(className)}
        </Link>
      </li>
    );
  } else {
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
        {label}
        {addIcons(className)}
      </li>
    );
  }
}
