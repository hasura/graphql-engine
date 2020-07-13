import styled from 'styled-components';
import {
  color,
  typography,
  layout,
  space,
  position,
  ColorProps,
  TypographyProps,
  LayoutProps,
  SpaceProps,
} from 'styled-system';

export interface SvgProps
  extends Omit<
    React.ComponentPropsWithRef<'svg'>,
    | 'color'
    | 'opacity'
    | 'fontFamily'
    | 'fontSize'
    | 'fontStyle'
    | 'fontWeight'
    | 'letterSpacing'
    | 'overflow'
    | 'height'
    | 'display'
    | 'width'
  > {}

export const Svg = ('svg' as any) as React.FC<SvgProps>;

interface StyledIconOwnProps
  extends ColorProps,
    TypographyProps,
    LayoutProps,
    SpaceProps {
  pointer?: boolean;
}

export const StyledIcon = styled(Svg)<StyledIconOwnProps>`
  cursor: ${({ pointer }) => (pointer ? 'pointer' : '')};

  ${color}
  ${typography}
  ${layout}
  ${space}
  ${position}
`;

export interface StyledIconProps extends SvgProps, StyledIconOwnProps {}
