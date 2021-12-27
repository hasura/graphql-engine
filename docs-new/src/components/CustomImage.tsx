import React from 'react';
import Thumbnail from './Thumbnail/Thumbnail';
import {useActiveVersion, useLatestVersion} from '@theme/hooks/useDocs';
import useBaseUrl from '@docusaurus/useBaseUrl';
// import {usePluginData} from '@docusaurus/useGlobalData';

// Ref: https://github.com/facebook/docusaurus/issues/2302#issuecomment-674175068

const CustomImage = ({src, ...restProps}) => {
  const activeVersion = useActiveVersion('default');
  const latestVersion = useLatestVersion('default');
  // const myPluginData = usePluginData('docusaurus-plugin-content-docs');
  const pathPrefix = activeVersion.path === latestVersion.path ? "docs" : `versioned_docs/version-${activeVersion.name}`;
  const versionedSrcPath = `@site/${pathPrefix}${src}`;
  const versionedSrc = versionedSrcPath;

  return <img src={versionedSrc} {...restProps} />;
};

export default CustomImage;