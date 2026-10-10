import clsx from 'clsx';
import { FaQuestionCircle } from 'react-icons/fa';
import {
  activeLinkStyle,
  itemContainerStyle,
  linkStyle,
} from './HeaderNavItem';
import { Text } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

export const Help = ({ isSelected }: { isSelected?: boolean }) => {
  return (
    <div className={itemContainerStyle}>
      <a
        id="help"
        className={clsx(linkStyle, isSelected && activeLinkStyle)}
        href="https://hasura.io/help"
        target="_blank"
        rel="noopener noreferrer"
      >
        <Flex align="center" gap="2">
          <FaQuestionCircle className="w-3 h-3" />
          <Text size="1" className="uppercase">
            HELP
          </Text>
        </Flex>
      </a>
    </div>
  );
};
