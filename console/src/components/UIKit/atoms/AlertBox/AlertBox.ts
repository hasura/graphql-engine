import styled from 'styled-components';
import {
  flexbox,
  color,
  border,
  typography,
  layout,
  space,
  shadow,
  FlexboxProps,
  ColorProps,
  BorderProps,
  TypographyProps,
  LayoutProps,
  SpaceProps,
  ShadowProps,
} from 'styled-system';

import { BoxProps, Box } from '../Box';

interface StyledAlertBoxOwnProps
  extends FlexboxProps,
    ColorProps,
    BorderProps,
    TypographyProps,
    LayoutProps,
    SpaceProps,
    ShadowProps {}

export interface StyledAlertBoxProps extends StyledAlertBoxOwnProps, BoxProps {}

export const StyledAlertBox = styled(Box)<StyledAlertBoxProps>`
  ${flexbox};
  ${color}
  ${border}
  ${typography}
  ${layout}
  ${space}
  ${shadow}

  /* Alert type text */
  span {
    text-transform: capitalize;
  }
`;
