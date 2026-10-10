import { Avatar, Flex } from '@radix-ui/themes';
import { FaLink } from 'react-icons/fa';
import { Text } from '../../typography';

export const LinkBlockVertical = ({ title }: { title: string }) => {
  return (
    <Flex align="center" className="w-full ml-6 border-l border-gray-300 py-6">
      <Avatar radius="full" fallback={<FaLink />} className="mr-4 ml-[-20px]" />
      <Text weight="medium" color="gray">
        {title}
      </Text>
    </Flex>
  );
};
