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
  
  /* Pseudo classes like hover are not supported by Styled-System  */

  /* Hover effects when button is not disabled */

  ${props => {
    const { type, theme, disabled } = props;

    // Default ~ If the received button type is out of the theme range then default box-shadow color value will be assigned.

    const boxShadowColor = theme.buttons[type]
      ? theme.buttons[type].boxShadowColor
      : theme.buttons.default.boxShadowColor;

    // In case of Secondary Button when it's not disabled

    if (type === 'secondary' && !disabled) {
      return css`
        &:hover {
          box-shadow: ${`0 2px 8px 0 ${boxShadowColor}`};
          background: ${theme.colors.black.secondary};
          color: ${theme.colors.white};
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
  
  /* Button cursor ~ disabled state */
   
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
