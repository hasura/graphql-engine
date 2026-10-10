import { Avatar, Flex } from '@radix-ui/themes';
import { FaLink } from 'react-icons/fa';

export const LinkBlockHorizontal = () => {
  return (
    <Flex align="center" justify="center" className="relative w-full py-4">
      <Avatar
        className="z-10"
        style={{
          height: 32,
          width: 32,
          borderRadius: 100,
        }}
        fallback={<FaLink />}
      />
      <div className="absolute w-full border-b border-gray-300" />
    </Flex>
  );
};
