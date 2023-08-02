import clsx from 'clsx';
import React from 'react';
import { FaSearch } from 'react-icons/fa';

export const SearchBar = (props: React.HTMLProps<HTMLInputElement>) => {
  return (
    <div className={clsx('flex relative w-full')}>
      <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
        <FaSearch className="h-5 w-5 text-gray-400" />
      </div>
      <input
        type="text"
        className={clsx(
          'pl-10 block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400 placeholder-gray-500'
        )}
        {...props}
      />
    </div>
  );
};
