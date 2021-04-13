import React from 'react';

import { getCurrentPageHost } from '../utils';

import styles from '../RESTStyles.scss';

type URLPreviewProps = {
  urlInput: string;
};

const URLPreview: React.FC<URLPreviewProps> = ({ urlInput }) => {
  const endText = getCurrentPageHost();

  return (
    <div className={styles.url_preview_layout}>
      <p className={styles.url_preview}>
        {endText}/api/rest/{urlInput}
      </p>
    </div>
  );
};

export default URLPreview;
