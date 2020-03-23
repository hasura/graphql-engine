import styled from 'styled-components';
import { color, typography, layout, space } from 'styled-system';

export const StyledIcon = styled.svg`
  cursor: ${({ cursor }) => (cursor ? 'pointer' : '')};

  ${color}
  ${typography}
  ${layout}
  ${space}
`;
