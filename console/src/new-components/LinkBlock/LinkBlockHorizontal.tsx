import React from 'react';
import { FaLink } from 'react-icons/fa';

export const LinkBlockHorizontal = () => {
  return (
    <div className="col-span-2 flex relative items-center justify-center w-full py-md">
      <div
        className="flex z-10 items-center justify-center border border-gray-300 bg-white"
        style={{
          height: 32,
          width: 32,
          borderRadius: 100,
        }}
      >
        <FaLink />
      </div>
      <div className="absolute w-full border-b border-gray-300" />
    </div>
  );
};
