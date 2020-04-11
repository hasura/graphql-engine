import styled from 'styled-components';
import { typography, color, space, border, layout } from 'styled-system';

export const StyledTextLink = styled.a`
  cursor: pointer;

  &&& {
    text-decoration: none;
  }

  ${typography}
  ${color}
  ${space}
  ${border}
  ${layout}
`;
