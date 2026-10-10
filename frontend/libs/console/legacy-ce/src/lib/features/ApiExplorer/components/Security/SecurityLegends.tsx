import { Flex } from '@radix-ui/themes';
import { IconButton, Text } from '@hasura/shared/ui';
import { FaCheck, FaGlobe, FaTimes } from 'react-icons/fa';

export const Legends = {
  Enabled: () => (
    <IconButton variant="ghost" radius="full" icon={FaCheck} color="green" />
  ),
  Disabled: () => (
    <IconButton variant="ghost" radius="full" icon={FaTimes} color="red" />
  ),
  Global: () => (
    <IconButton variant="ghost" radius="full" icon={FaGlobe} color="gray" />
  ),
};

const SecurityLegends = ({ className }: { className?: string }) => (
  <Flex align="center" gap="4" className={className}>
    <Flex align="center" gap="1">
      <Legends.Enabled />
      <Text>enabled</Text>
    </Flex>
    <Flex align="center" gap="1">
      <Legends.Disabled />
      <Text>disabled</Text>
    </Flex>
    <Flex align="center" gap="1">
      <Legends.Global />
      <Text>global setting</Text>
    </Flex>
    <Text color="gray">Click a row to edit.</Text>
  </Flex>
);

export default SecurityLegends;
