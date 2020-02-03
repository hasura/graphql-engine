import styled, { css } from 'styled-components';
import { layout, space, color, border, typography } from 'styled-system';

export const ButtonStyles = styled.button`
  cursor: pointer;
  appearance: button;

  /* Base font color values */

  color: ${({ type, theme }) => {
    // color for primary and secondary button ~ black.text
    if (type === 'primary' || type === 'secondary') {
      return theme.colors.black.text;
    }
    // No else clause here.

    // default color for rest of the buttons ~ white.
    return theme.colors.white;
  }};

  /* Hover effects */

  ${({ type, theme: { colors } }) => {
    const boxShadowColorsObject = {
      primary: colors.yellow.hover,
      secondary: colors.black.hover,
      success: colors.green.hover,
      danger: colors.red.hover,
      warning: colors.orange.hover,
      info: colors.blue.hover,
    };

    // Default ~ If by mistake specified button type is out of the theme range then default box-shadow color value will be black.hover.

    const boxShadowColor = boxShadowColorsObject[type]
      ? boxShadowColorsObject[type]
      : colors.black.hover;

    // In case of Secondary Button

    if (type === 'secondary') {
      return css`
        &:hover {
          box-shadow: ${`0 2px 8px 0 ${boxShadowColor}`};
          background: ${colors.black.secondary};
          color: ${colors.white};
        }
      `;
    }
    // No else clause here.

    // Hover effect for the reset of buttons except secondary.

    return css`
      &:hover {
        box-shadow: ${`0 2px 8px 0 ${boxShadowColor}`};
      }
    `;
  }}

  /* Styled-System ********************/
    ${layout}
    ${space}
    ${typography}
    ${color}
    ${border}
`;
