import { createGlobalStyle } from 'styled-components';

const BaseStyles = createGlobalStyle(
  props => `
  /* Demo purpose */

   h3 {
    margin: 2.5rem 0;
    color: ${props.theme.colors.black.text};
  }
`
);

export default BaseStyles;
