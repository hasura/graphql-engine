import styled, { css } from 'styled-components';
import { layout, space, color, border, typography } from 'styled-system';

export const ButtonStyles = styled.button`
  cursor: ${({ disabled }) => !disabled && 'pointer'};
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

  /* Hover effects when button is not disabled */

  ${({ type, theme: { colors }, disabled }) => {
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

    // In case of Secondary Button when it's not disabled

    if (type === 'secondary' && !disabled) {
      return css`
        &:hover {
          box-shadow: ${`0 2px 8px 0 ${boxShadowColor}`};
          background: ${colors.black.secondary};
          color: ${colors.white};
        }
      `;
    } else if (!disabled) {
      // Hover effect for the reset of buttons except secondary in case of when button is not disabled.
      return css`
        &:hover {
          box-shadow: ${`0 2px 8px 0 ${boxShadowColor}`};
        }
      `;
    }
    // No else clause here.
  }}
   
   &:disabled {
       cursor: not-allowed;
   }

  /* Styled-System ********************/
    ${layout}
    ${space}
    ${typography}
    ${color}
    ${border}
`;
