import styled from 'styled-components';
import { typography, color, space, border } from 'styled-system';

export const StyledHeading = styled.h1`
    ${typography}
    ${color}
    ${space}
`;

export const StyledText = styled.p`
    ${typography}
    ${color}
    ${space}
    ${border}
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
