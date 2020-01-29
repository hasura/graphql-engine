import styled from 'styled-components';
import { fontSize, fontWeight, color, border, space } from 'styled-system';

// Paragraph ************************* //

export const Text = styled.p`
    ${fontSize}
    ${fontWeight}
    ${color}
    ${space}
    ${border}  
`;

// Heading ************************* //

export const Heading = styled.h1`
    ${fontSize}
    ${fontWeight}
    ${color}
    ${space}
`;
