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
} from 'styled-system';

export const StyledHeading = styled.h1<
  TypographyProps & ColorProps & SpaceProps
>`
  ${typography}
  ${color}
    ${space}
`;

export const StyledText = styled.p<
  TypographyProps & ColorProps & SpaceProps & BorderProps
>`
  ${typography}
  ${color}
    ${space}
    ${border}
`;

export const StyledTextLink = styled.a<
  TypographyProps & ColorProps & SpaceProps & BorderProps
>`
  &&& {
    text-decoration: none;
    color: inherit;
  }

  cursor: pointer;

  ${typography}
  ${color}
  ${space}
  ${border}
`;
