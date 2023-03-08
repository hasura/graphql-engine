import { IconTooltip } from '../../../../../new-components/Tooltip';
import React from 'react';

export const SectionHeader: React.VFC<{ header: string; tip: string }> = ({
  header,
  tip,
}) => (
  <div>
    <div className="flex items-center my-4 text-gray-600 font-semibold">
      {header}
      <IconTooltip message={tip} />
    </div>
  </div>
);
