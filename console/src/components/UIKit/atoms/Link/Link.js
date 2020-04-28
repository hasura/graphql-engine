import styled from 'styled-components';
import { typography, color, space, border, layout } from 'styled-system';

export const StyledLink = styled.a`
  cursor: pointer;

  &&& {
    text-decoration: none;
    color: inherit;
  }

  ${typography}
  ${color}
  ${space}
  ${border}
  ${layout}
`;
