import React from 'react';
import { FaLink } from 'react-icons/fa';

export const LinkBlockVertical = ({ title }: { title: string }) => {
  return (
    <div className="flex items-center w-full ml-lg border-l border-gray-300 py-lg">
      <div
        className="flex items-center justify-center border border-gray-300 bg-white mr-md"
        style={{
          height: 32,
          width: 32,
          marginLeft: -17,
          borderRadius: 100,
        }}
      >
        <FaLink />
      </div>
      <p className="font-semibold text-muted">{title}</p>
    </div>
  );
};
