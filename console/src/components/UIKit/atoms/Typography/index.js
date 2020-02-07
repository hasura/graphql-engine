import styled from 'styled-components';
import { typography, color, space, border } from 'styled-system';

// Heading ************************* //

export const Heading = styled.h1`
    ${typography}
    ${color}
    ${space}
`;

// Paragraph ************************* //

export const Text = styled.p`
    ${typography}
    ${color}
    ${space}
    ${border}
`;
