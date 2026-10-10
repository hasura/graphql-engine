import { FaProjectDiagram } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';

type RootFieldTitleProps = {
  title: string;
};

export const RootFieldTitle = ({ title }: RootFieldTitleProps) => (
  <Flex
    align="center"
    className="font-semibold cursor-pointer w-max whitespace-nowrap"
  >
    <FaProjectDiagram className="mr-1 h-4 w-5" /> {title}
  </Flex>
);
