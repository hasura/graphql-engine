import styled from 'styled-components';
import {
  space,
  layout,
  flexbox,
  border,
  color,
  position,
  FlexboxProps,
  SpaceProps,
  LayoutProps,
  BorderProps,
  ColorProps,
  PositionProps,
} from 'styled-system';

import { Box, BoxProps } from '../Box';

interface FlexOwnProps
  extends BoxProps,
    FlexboxProps,
    SpaceProps,
    LayoutProps,
    BorderProps,
    ColorProps,
    PositionProps {}

export const Flex = styled(Box)<FlexOwnProps>`
  display: flex;
  ${color}
  ${space}
  ${layout}
  ${flexbox}
  ${border}
  ${position}
`;
