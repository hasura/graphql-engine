import styled from 'styled-components';
import {
  color,
  border,
  typography,
  layout,
  space,
  shadow,
  ColorProps,
  BorderProps,
  TypographyProps,
  LayoutProps,
  SpaceProps,
  ShadowProps,
} from 'styled-system';
import { SpinnerProps } from '.';

export const StyledSpinner = styled.div<
  SpinnerProps &
    ColorProps &
    BorderProps &
    TypographyProps &
    LayoutProps &
    SpaceProps &
    ShadowProps
>`
  position: relative;

  ${color}
  ${border}
  ${typography}
  ${layout}
  ${space}
  ${shadow}
`;
