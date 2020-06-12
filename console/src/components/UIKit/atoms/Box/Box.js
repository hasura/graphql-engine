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

export const Box = styled.div`
  cursor: ${props => (props.pointer ? 'pointer' : '')};
  ${color}
  ${border}
  ${typography}
  ${layout}
  ${space}
  ${shadow}
  ${background}
  ${position}
`;
