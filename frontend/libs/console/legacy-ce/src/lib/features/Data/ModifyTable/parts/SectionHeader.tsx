import React from 'react';

export const SectionHeader: React.FC = ({ children }) => (
  <h4 className="flex text-lg items-center text-gray-600 font-semibold ">
    {children}
  </h4>
);
