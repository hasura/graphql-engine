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
} from 'styled-system';

export const StyledBadge = styled.span`
  text-transform: uppercase;
  letter-spacing: 0.7px;
  font-family: Roboto;
  font-weight: 500;

  ${color}
  ${border}
  ${typography}
  ${layout}
  ${space}
  ${shadow}
  ${background}
  ${position}
`;
