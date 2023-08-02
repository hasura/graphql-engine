import { IconTooltip } from '../../../../new-components/Tooltip';
import React from 'react';
import { SectionHeader } from './SectionHeader';

export const Section: React.FC<{
  headerText: string;
  tooltipMessage?: string;
}> = ({ children, headerText, tooltipMessage }) => (
  <div className="mb-8">
    <div className="flex flex-row items-center mb-formlabel">
      <SectionHeader>{headerText}</SectionHeader>
      {!!tooltipMessage && <IconTooltip message={tooltipMessage} />}
    </div>
    {children}
  </div>
);
