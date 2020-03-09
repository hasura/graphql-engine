import styled from 'styled-components';
import { typography, color, space, border } from 'styled-system';

// Heading ************************* //

const Heading = styled.h1`
    ${typography}
    ${color}
    ${space}
`;

// Default Props for Heading

Heading.defaultProps = {
  color: 'black.text',
};

// ******************************* //

const TextStyles = styled.p`
    ${typography}
    ${color}
    ${space}
    ${border}
`;

// TextLink ************* //

const TextLinkStyles = styled.a`
  text-decoration: none !important;
  cursor: pointer;

  /* Styled-System */
      ${typography}
      ${color}
      ${space}
      ${border}
`;

// ******************************************* //

export { Heading, TextStyles, TextLinkStyles };
