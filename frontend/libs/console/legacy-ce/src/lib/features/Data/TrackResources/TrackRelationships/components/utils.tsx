import { Flex } from '@radix-ui/themes';
import { FaMagic } from 'react-icons/fa';

export const getTableHeaderRow = (checkAllElement: React.ReactElement<any>) => [
  checkAllElement,
  <Flex key="suggested-relationships" align="center" gap="2">
    <FaMagic /> SUGGESTED RELATIONSHIPS
  </Flex>,
  'SOURCE',
  'TYPE',
  'RELATIONSHIP',
  'ACTIONS',
];
