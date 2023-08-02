import React, { useEffect, useState } from 'react';
import EnterpriseDark from '@site/static/icons/enterprise-light.svg';
import EnterpriseLight from '@site/static/icons/enterprise-dark.svg';
import CloudDark from '@site/static/icons/cloud-light.svg';
import CloudLight from '@site/static/icons/cloud-dark.svg';
import BetaTag from '@site/src/components/BetaTag/BetaTag';
import styles from '@site/src/theme/DocSidebarItem/Category/styles.module.scss';
import { useColorMode } from '@docusaurus/theme-common';

export function addIconsToLabel(label, className) {
  const { colorMode } = useColorMode();
  const [definedColorMode, setDefinedColorMode] = useState('');

  useEffect(() => {
    setDefinedColorMode(colorMode);
  }, [colorMode]);

  const isDarkMode = definedColorMode === 'dark';

  const enterpriseIcon = isDarkMode ? <EnterpriseDark /> : <EnterpriseLight />;
  const cloudIcon = isDarkMode ? <CloudDark /> : <CloudLight />;
  const betaIcon = <BetaTag />;

  // Conditional rendering for sidebar icons
  let icons;
  switch (className) {
    case 'enterprise-icon':
      icons = enterpriseIcon;
      break;
    case 'cloud-icon':
      icons = cloudIcon;
      break;
    case 'enterprise-icon-and-beta':
      icons = (
        <>
          {enterpriseIcon} {betaIcon}
        </>
      );
      break;
    case 'cloud-and-enterprise-icon':
      icons = (
        <>
          {cloudIcon} {enterpriseIcon}
        </>
      );
      break;
    case 'beta-icon':
      icons = betaIcon;
      break;
  }

  return (
    <div className={styles['sidebar_link_wrapper']}>
      {label} {icons}
    </div>
  );
}
