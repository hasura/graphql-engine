import styled from 'styled-components';
import { typography, color, space, border } from 'styled-system';

interface HeadingProps {
  mb: string;
  color?: string;
}
export const StyledHeading = styled.h1<HeadingProps>`
    ${typography}
    ${color}
    ${space}
`;

interface StyledTextProps {
  lineHeight: string;
  fontSize: string;
  fontWeight?: string;
  color: string;
}
export const StyledText = styled.p<StyledTextProps>`
    ${typography}
    ${color}
    ${space}
    ${border}
`;

interface StyledTextLinkProps {
  borderBottom: any;
  borderColor: string;
  fontWeight: string;
  fontSize: string;
}
export const StyledTextLink = styled.a<StyledTextLinkProps>`
  &&& {
    text-decoration: none;
  }

  cursor: pointer;

  ${typography}
  ${color}
  ${space}
  ${border}
`;
