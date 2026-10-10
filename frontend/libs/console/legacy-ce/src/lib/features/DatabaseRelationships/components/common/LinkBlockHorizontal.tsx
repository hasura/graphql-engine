import React from 'react';
import { FaLink } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';

export const LinkBlockHorizontal = () => {
  return (
    <Flex
      align="center"
      justify="center"
      className="col-span-2 relative w-full py-4"
    >
      <Flex
        align="center"
        justify="center"
        className="z-10 border border-gray-300 bg-white"
        style={{
          height: 32,
          width: 32,
          borderRadius: 100,
        }}
      >
        <FaLink />
      </Flex>
      <div className="absolute w-full border-b border-gray-300" />
    </Flex>
  );
};
