import React from 'react';

export const LeftContainer: React.FC<{ children?: React.ReactNode }> = ({
  children,
}) => {
  return (
    <div
      id="nav-sidebar"
      style={{
        height: 'calc(100vh - 50px)',
      }}
    >
      {children}
    </div>
  );
};
