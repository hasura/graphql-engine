import React from 'react';
import { SectionHeader } from './SectionHeader';

export const Section: React.FC<{ headerText: string }> = ({
  children,
  headerText,
}) => (
  <div className="mb-8">
    <SectionHeader>{headerText}</SectionHeader>
    {children}
  </div>
);
