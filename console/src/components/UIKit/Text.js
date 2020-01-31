import styled from 'styled-components';
import { typography, color, border, space } from 'styled-system';

// Paragraph ************************* //

export const Text = styled.p`
    ${typography}
    ${color}
    ${space}
    ${border}  
`;

// Heading ************************* //

export const Heading = styled.h1`
    ${typography}
    ${color}
    ${space}
`;
