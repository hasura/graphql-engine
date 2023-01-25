import styled from 'styled-components';
import {
  color,
  border,
  typography,
  layout,
  space,
  background,
  shadow,
  position,
  ColorProps,
  BorderProps,
  TypographyProps,
  LayoutProps,
  SpaceProps,
  BackgroundProps,
  ShadowProps,
  PositionProps,
} from 'styled-system';

export interface StyledOwnBadgeProps
  extends ColorProps,
    BorderProps,
    TypographyProps,
    LayoutProps,
    SpaceProps,
    BackgroundProps,
    ShadowProps,
    PositionProps,
    Omit<React.ComponentPropsWithRef<'span'>, 'color'> {}

const StyledBadge = styled.span<StyledOwnBadgeProps>`
  text-transform: uppercase;
  letter-spacing: 0.4px;
  ${color}
  ${border}
  ${typography}
  ${layout}
  ${space}
  ${shadow}
  ${background}
  ${position}
`;

export default StyledBadge;
