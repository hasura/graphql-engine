import React from 'react';
import { useEffect, useState } from 'react';
import { useColorMode } from '@docusaurus/theme-common';
import CloudLight from '@site/static/icons/cloud-light.svg';
import CloudDark from '@site/static/icons/cloud-dark.svg';
import EnterpriseLight from '@site/static/icons/enterprise-light.svg';
import EnterpriseDark from '@site/static/icons/enterprise-dark.svg';
// TODO: Update these with correct file imports
import CloudEELight from '@site/static/icons/ce_ee_light.svg';
import CloudEEDark from '@site/static/icons/ce_ee_dark.svg';
import styles from './styles.module.scss';

const HeadingIcon = ({ icon, size }) => {
  const { colorMode, setColorMode } = useColorMode();
  const [iconTheme, setIconTheme] = useState('');

  useEffect(() => {
    setIconTheme(colorMode === 'dark' ? 'dark' : 'light');
  }, [colorMode]);

  return (
    <div className={`${styles['icon-container']}`}>
      {icon === `cloud` && iconTheme === `dark` && <CloudLight style={{ width: size, height: size }} />}
      {icon === `cloud` && iconTheme === `light` && <CloudDark style={{ width: size, height: size }} />}
      {icon === `ee` && iconTheme === `dark` && <EnterpriseLight style={{ width: size, height: size }} />}
      {icon === `ee` && iconTheme === `light` && <EnterpriseDark style={{ width: size, height: size }} />}
      {icon === `cloud-ee` && iconTheme === `light` && <CloudEEDark style={{ width: size, height: size }} />}
      {icon === `cloud-ee` && iconTheme === `dark` && <CloudEELight style={{ width: size, height: size }} />}
    </div>
  );
};

export default HeadingIcon;
