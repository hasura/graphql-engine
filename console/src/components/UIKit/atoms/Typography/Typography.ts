import styled from 'styled-components';
import {
  typography,
  color,
  space,
  border,
  TypographyProps,
  ColorProps,
  SpaceProps,
  BorderProps,
  layout,
} from 'styled-system';

export const StyledHeading = styled.h1<
  TypographyProps & ColorProps & SpaceProps
>`
    ${typography}
    ${color}
    ${space}
    ${layout}
    ${border}
`;

export const StyledText = styled.p<
  TypographyProps & ColorProps & SpaceProps & BorderProps
>`
    ${typography}
    ${color}
    ${space}
    ${border}
    ${layout}
`;
