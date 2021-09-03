import React from 'react';

import { getCurrentPageHost } from '../utils';

type URLPreviewProps = {
  urlInput: string;
};

const URLPreview: React.FC<URLPreviewProps> = ({ urlInput }) => {
  const endText = getCurrentPageHost();

  return (
    <div>
      <p>
        {endText}/api/rest/{urlInput}
      </p>
    </div>
  );
};

export default URLPreview;
