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
  ShadowProps
} from 'styled-system';

import { AlertBoxProps } from './index';

export const StyledAlertBox = styled.div<AlertBoxProps & FlexboxProps & ColorProps & BorderProps & TypographyProps & LayoutProps & SpaceProps & ShadowProps>`
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
