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

export type StyledSpinnerProps = ColorProps &
  BorderProps &
  TypographyProps &
  LayoutProps &
  SpaceProps &
  ShadowProps;

export const StyledSpinner = styled.div<StyledSpinnerProps>`
  position: relative;

  ${color}
  ${border}
  ${typography}
  ${layout}
  ${space}
  ${shadow}
`;
