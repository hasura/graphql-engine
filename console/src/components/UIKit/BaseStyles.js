import { createGlobalStyle } from 'styled-components';

const BaseStyles = createGlobalStyle(
  props => `
  /* Demo purpose */

  h1, h3 {
    margin: 2.5rem 0;
    color: ${props.theme.colors.black.text};
  }
`
);

export default BaseStyles;
