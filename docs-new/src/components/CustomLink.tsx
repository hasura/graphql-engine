// Ref: https://github.com/facebook/docusaurus/issues/2302#issuecomment-674175068

import React from 'react';
import Link from '@docusaurus/Link';
import {useActiveVersion, useLatestVersion} from '@theme/hooks/useDocs';

const CustomMDXLink = ({to, children}) => {
  const activeVersion = useActiveVersion('default');
  const latestVersion = useLatestVersion('default');
  const pathPrefix = (activeVersion.path === latestVersion.path) || to.startsWith("#") ? "" : activeVersion.path;
  const versionedPath = `${pathPrefix}${to}`;
  

  return <Link to={versionedPath}>{children}</Link>;
};

export default CustomMDXLink;