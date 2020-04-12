import styled from 'styled-components';
import { typography, color, space, border, layout } from 'styled-system';

export const StyledHeading = styled.h1`
    ${typography}
    ${color}
    ${space}
    ${layout}
    ${border}
`;

export const StyledText = styled.p`
    ${typography}
    ${color}
    ${space}
    ${border}
    ${layout}
`;
