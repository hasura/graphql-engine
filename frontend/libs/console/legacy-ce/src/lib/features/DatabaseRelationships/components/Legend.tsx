import { Text } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';
import { IconType } from 'react-icons';
import { FaColumns, FaDatabase, FaFont, FaPlug, FaTable } from 'react-icons/fa';
import { FiType } from 'react-icons/fi';

const legend: { Icon: IconType; name: string }[] = [
  {
    Icon: FaPlug,
    name: 'Remote Schema',
  },
  {
    Icon: FiType,
    name: 'Type',
  },
  {
    Icon: FaFont,
    name: 'Field',
  },
  {
    Icon: FaDatabase,
    name: 'Database',
  },
  {
    Icon: FaTable,
    name: 'Table',
  },
  {
    Icon: FaColumns,
    name: 'Column',
  },
];

const Legend = () => {
  return (
    <Flex justify="end" align="center" gap="2">
      {legend.map((item) => {
        const { Icon, name } = item;
        return (
          <Flex align="center" gap="1" key={name}>
            <Icon />
            <Text>{name}</Text>
          </Flex>
        );
      })}
    </Flex>
  );
};

export default Legend;
