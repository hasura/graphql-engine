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

interface BoxProps extends Omit<React.ComponentPropsWithRef<'div'>, 'color'> {}
const Box = ('div' as any) as React.FC<BoxProps>;

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
