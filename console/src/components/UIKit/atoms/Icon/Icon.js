import styled from 'styled-components';
import { color, typography, layout, space } from 'styled-system';

export const StyledIcon = styled.svg`
  cursor: ${({ pointer }) => (pointer ? 'pointer' : '')};

  ${color}
  ${typography}
  ${layout}
  ${space}
`;
