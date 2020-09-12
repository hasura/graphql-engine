import styled from 'styled-components';
import {
  border,
  BorderProps,
  color,
  ColorProps,
  layout,
  LayoutProps,
  shadow,
  ShadowProps,
  space,
  SpaceProps,
  typography,
  TypographyProps,
} from 'styled-system';
import { Box, BoxProps } from '../Box';

interface StyledSpinnerOwnProps
  extends ColorProps,
    BorderProps,
    TypographyProps,
    LayoutProps,
    SpaceProps,
    ShadowProps {}

export const StyledSpinner = styled(Box)<StyledSpinnerOwnProps>`
  position: relative;

  ${color}
  ${border}
  ${typography}
  ${layout}
  ${space}
  ${shadow}
`;

export interface StyledSpinnerProps extends BoxProps, StyledSpinnerOwnProps {}
