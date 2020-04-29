import styled from 'styled-components';
import { typography, color, space, border, layout } from 'styled-system';

export const StyledLink = styled.a`
  cursor: pointer;
  color: inherit;

  &&& {
    text-decoration: none;
  }

  ${typography}
  ${color}
  ${space}
  ${border}
  ${layout}
`;
