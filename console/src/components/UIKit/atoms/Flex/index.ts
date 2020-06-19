import styled from 'styled-components';
import {
  space,
  layout,
  flexbox,
  border,
  color,
  FlexboxProps,
  SpaceProps,
  LayoutProps,
  BorderProps,
  ColorProps
} from 'styled-system';

import { Box, BoxProps } from '../Box';

interface FlexOwnProps
  extends BoxProps,
  FlexboxProps,
  SpaceProps,
  LayoutProps,
  BorderProps,
  ColorProps { }

export const Flex = styled(Box) <FlexOwnProps>`
  display: flex;
  align-items: center;
  ${color}
  ${space}
  ${layout}
  ${flexbox}
  ${border}
`;
