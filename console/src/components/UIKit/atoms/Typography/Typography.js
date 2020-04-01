import styled from 'styled-components';
import { typography, color, space, border, layout } from 'styled-system';

export const StyledHeading = styled.h1`
    ${typography}
    ${color}
    ${space}
    ${layout}
`;

export const StyledText = styled.p`
    ${typography}
    ${color}
    ${space}
    ${border}
    ${layout}
`;

export const StyledTextLink = styled.a`
  &&& {
    text-decoration: none;
  }

  cursor: pointer;

  ${typography}
  ${color}
  ${space}
  ${border}
`;
