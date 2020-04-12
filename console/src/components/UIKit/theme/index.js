// Theme specification for the Design-System.

const colors = {
  red: {
    original: '#ff0000',
    primary: '#e53935',
    hover: 'rgba(229, 57, 53, 0.4)',
    light: '#f7e9e9'
  },
  green: {
    original: '#008000',
    primary: '#69cb43',
    hover: 'rgba(123, 179, 66, 0.4)',
    light: '#f0f8e7'
  },
  blue: {
    original: '#0000ff',
    primary: '#1f88e5',
    hover: 'rgba(31, 136, 229, 0.4)',
    light: '#f0f8ff'
  },
  orange: {
    original: '#ffa500',
    primary: '#fdb02c',
    hover: 'rgba(253, 176, 44, 0.4)',
    light: '#fff8ed'
  },
  yellow: {
    original: '#ffff00',
    primary: '#f8d721',
    hover: 'rgba(204, 177, 25, 0.4)'
  },
  grey: {
    original: '#888888',
    tab: '#939390',
    border: '#ededed'
  },
  black: {
    original: '#000',
    secondary: '#484538',
    text: '#4d4d4d',
    hover: 'rgba(0, 0, 0, 0.16)'
  },
  white: '#fff',
  transparent: 'transparent',
  tab: '#1fd6e5'
};

// ********************************** //

const button = {
  primary: {
    backgroundColor: colors.yellow.primary,
    boxShadowColor: colors.yellow.hover,
    color: colors.black.text
  },
  secondary: {
    backgroundColor: colors.white,
    boxShadowColor: colors.black.hover,
    color: colors.black.text
  },
  success: {
    backgroundColor: colors.green.primary,
    boxShadowColor: colors.green.hover,
    color: colors.white
  },
  danger: {
    backgroundColor: colors.red.primary,
    boxShadowColor: colors.red.hover,
    color: colors.white
  },
  warning: {
    backgroundColor: colors.orange.primary,
    boxShadowColor: colors.orange.hover,
    color: colors.white
  },
  info: {
    backgroundColor: colors.blue.primary,
    boxShadowColor: colors.blue.hover,
    color: colors.white
  },
  default: {
    backgroundColor: colors.yellow.primary,
    boxShadowColor: colors.black.hover,
    color: colors.white
  }
};

// ********************************** //

const alertBox = {
  success: {
    backgroundColor: colors.green.light,
    borderColor: colors.green.primary,
    message: 'You did something awesome. Well done!'
  },
  info: {
    backgroundColor: colors.blue.light,
    borderColor: colors.blue.primary,
    message: 'You need to do something.'
  },
  warning: {
    backgroundColor: colors.orange.light,
    borderColor: colors.orange.primary,
    message: 'You are about to do something wrong.'
  },
  error: {
    backgroundColor: colors.red.light,
    borderColor: colors.red.primary,
    message: 'You did something wrong.'
  },
  default: {
    backgroundColor: colors.green.light,
    borderColor: colors.green.primary,
    message: ''
  }
};

// ********************************** //

const icon = {
  success: {
    color: colors.green.primary
  },
  info: {
    color: colors.blue.primary
  },
  warning: {
    color: colors.orange.primary
  },
  error: {
    color: colors.red.primary
  },
  // type ~ out of range
  default: {
    color: colors.black.secondary
  }
};

// Border Radius ********************* //

const radii = [0, 2, 4, 8, 12, 16];

/* border-radius aliases
 * xs: 2px (extra small)
 * sm: 4px (small)
 * md: 8px (medium)
 * lg: 12px (large)
 * xl: 16px (extra large)
 * circle: 1000px
 */

radii.xs = radii[1];

radii.sm = radii[2];

radii.md = radii[3];

radii.lg = radii[4];

radii.xl = radii[5];

radii.circle = 1000;

// ********************************** //

const fontWeights = [0, 100, 200, 300, 400, 500, 600, 700, 800, 900];

/* font-weight aliases
 * normal: 400
 * medium: 500
 * bold: 700
 */

fontWeights.normal = fontWeights[4];

fontWeights.medium = fontWeights[5];

fontWeights.bold = fontWeights[7];

// ********************************** //

const fontSizes = [12, 14, 16, 18, 20, 24, 30, 36, 48, 80, 96];

/* font-sizes aliases
 * h1: 30px
 * h2: 24px
 * h3: 20px
 * h4: 18px
 * p: 16px
 * button: 14px
 * explain (Explainer Text): 12px
 * icon: 20px
 */

fontSizes.h1 = fontSizes[6];

fontSizes.h2 = fontSizes[5];

fontSizes.h3 = fontSizes[4];

fontSizes.h4 = fontSizes[3];

fontSizes.p = fontSizes[2];

fontSizes.button = fontSizes[1];

fontSizes.tab = fontSizes[3];

fontSizes.explain = fontSizes[0];

fontSizes.icon = fontSizes[3];

// ****************************** //

const space = [0, 4, 6, 8, 10, 12, 14, 16, 18, 20, 32, 64];

/* space ~ margin / padding aliases
 * zero: 0
 * xs: 4px (extra small)
 * sm: 8px (small)
 * md: 16px (medium)
 * lg: 32px (large)
 * xl: 64px (extra large)
 */

space.zero = space[0];

space.xs = space[1];

space.sm = space[3];

space.md = space[7];

space.lg = space[10];

space.xl = space[11];

// ********************************** //

const lineHeights = [1.33, 1.5];

/* line-height aliases
 * body: 1.5
 * explain: 1.3 ~ Explainer Text
 */

lineHeights.body = lineHeights[1];

lineHeights.explain = lineHeights[0];

// ********************************** //

/* sizes aliases (width & height)
 * sm: 40px
 * lg: 48px
 */

const sizes = [40, 48];

sizes.sm = sizes[0];

sizes.lg = sizes[1];

// ********************************** //

export const theme = {
  colors,
  radii,
  fonts: {
    roboto: 'Roboto'
  },
  fontWeights,
  fontSizes,
  sizes,
  space,
  lineHeights,
  shadows: [
    0,
    '0 0 3px 0 rgba(0, 0, 0, 0.16)',
    '0 3px 6px 0 rgba(0, 0, 0, 0.16)',
    '0 3px 10px 0 rgba(0, 0, 0, 0.16)',
    '0 7px 24px 0 rgba(0, 0, 0, 0.32)'
  ],
  borders: [0, '1px solid', '2px solid', '3px solid', '4px solid', '5px solid'],
  button,
  alertBox,
  icon
};
