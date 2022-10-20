import React from 'react';

export const SectionHeader: React.FC = ({ children }) => (
  <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
    {children}
  </h4>
);
