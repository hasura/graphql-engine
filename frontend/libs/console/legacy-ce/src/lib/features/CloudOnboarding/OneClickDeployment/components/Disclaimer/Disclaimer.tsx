import * as React from 'react';
import { FaInfoCircle } from 'react-icons/fa';

export const Disclaimer = () => {
  return (
    <div className="font-sans text-muted-dark text-sm">
      <FaInfoCircle className="mr-1 mb-1" />
      <span>
        Please use caution when installing projects provided by third parties.
        We recommend thoroughly reviewing the designated project on GitHub
        before installing.
      </span>
    </div>
  );
};
