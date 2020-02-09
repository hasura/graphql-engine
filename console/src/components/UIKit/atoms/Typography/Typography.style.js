import styled from 'styled-components';
import { typography, color, space, border } from 'styled-system';

// Heading ************************* //

export const Heading = styled.h1`
    ${typography}
    ${color}
    ${space}
`;

// Default Props for Heading

Heading.defaultProps = {
  color: 'black.text',
};

// Paragraph / Normal Text *********** //

export const Text = styled.p`
    ${typography}
    ${color}
    ${space}
    ${border}
`;

// Default Props for Text

Text.defaultProps = {
  color: 'black.text',
};

// TextLink ************* //

export const TextLinkStyles = styled.a`
  text-decoration: none !important;
  cursor: pointer;

  /* Styled-System */
      ${typography}
      ${color}
      ${space}
      ${border}
`;
