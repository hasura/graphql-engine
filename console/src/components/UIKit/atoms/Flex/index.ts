import styled from 'styled-components';
import {
  space,
  layout,
  flexbox,
  border,
  FlexboxProps,
  SpaceProps,
  LayoutProps,
  BorderProps,
} from 'styled-system';

import { Box, BoxProps } from '../Box';

interface FlexOwnProps
  extends BoxProps,
    FlexboxProps,
    SpaceProps,
    LayoutProps,
    BorderProps {}

export const Flex = styled(Box)<FlexOwnProps>`
  display: flex;
  align-items: center;
  padding: 0.35em;
  background-color: #f8f8f8;
  ${space}
  ${layout}
  ${flexbox}
  ${border}
`;
