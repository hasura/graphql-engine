import React from 'react';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { Flex, Box, FlexProps } from '@radix-ui/themes';
import { Separator } from '../components';

type PageContainerProps = FlexProps & {
  helmet?: string;
  leftContainer: React.ReactNode;
};

export const PageContainer: React.FC<PageContainerProps> = ({
  helmet,
  leftContainer,
  children,
  ...rest
}) => {
  useDocumentTitle(helmet);

  return (
    <Flex {...rest}>
      <Box className="w-1/5 h-full">{leftContainer}</Box>
      <Separator orientation="vertical" size="4" className="h-screen!" />
      <Box className="w-4/5">{children}</Box>
    </Flex>
  );
};
