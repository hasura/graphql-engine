import React from 'react';
import { HasuraLogoFull } from '../../../../new-components/HasuraLogo';

export function TopHeaderBar() {
  return (
    <header className="flex items-center bg-gray-700 px-sm py-xs">
      <div className="mr-auto ml-auto">
        <HasuraLogoFull size="sm" mode="primary" />
      </div>
    </header>
  );
}
