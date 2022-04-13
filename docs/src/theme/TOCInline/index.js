/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import React from 'react';
import styles from './styles.module.css';
import TOCItems from '@theme/TOCItems';
// In the event of re-swizzling, make sure to add the `filterTOC` and pass it down to TOCITems
export default function TOCInline({toc, minHeadingLevel, maxHeadingLevel, filterTOC}) {
  return (
    <div className={styles.tableOfContentsInline}>
      <TOCItems
        toc={toc}
        minHeadingLevel={minHeadingLevel}
        maxHeadingLevel={maxHeadingLevel}
        className="table-of-contents"
        linkClassName={null}
        // In the event of re-swizzling, make sure to add the `filterTOC` and pass it down to TOCITems
        filterTOC={filterTOC}
      />
    </div>
  );
}
