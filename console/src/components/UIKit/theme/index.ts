// Theme specification for the Design-System.

const colors = {
  red: {
    original: '#ff0000',
    primary: '#e53935',
    hover: 'rgba(229, 57, 53, 0.4)',
    light: '#f7e9e9',
  },
  green: {
    original: '#008000',
    primary: '#69cb43',
    hover: 'rgba(123, 179, 66, 0.4)',
    light: '#f0f8e7',
  },
  blue: {
    original: '#0000ff',
    primary: '#1f88e5',
    hover: 'rgba(31, 136, 229, 0.4)',
    light: '#f0f8ff',
  },
  orange: {
    original: '#ffa500',
    primary: '#fdb02c',
    hover: 'rgba(253, 176, 44, 0.4)',
    light: '#fff8ed',
  },
  yellow: {
    original: '#ffff00',
    primary: '#f8d721',
    hover: 'rgba(204, 177, 25, 0.4)',
  },
  grey: {
    original: '#888888',
    tab: '#939390',
    border: '#ededed',
  },
  black: {
    original: '#000',
    secondary: '#484538',
    text: '#292822',
    hover: 'rgba(0, 0, 0, 0.16)',
  },
  white: '#fff',
  transparent: 'transparent',
  tab: '#1fd6e5',
};

// ********************************** //

const button = {
  primary: {
    backgroundColor: colors.yellow.primary,
    boxShadowColor: colors.yellow.hover,
    color: colors.black.text,
  },
  secondary: {
    backgroundColor: colors.white,
    boxShadowColor: colors.black.hover,
    color: colors.black.text,
  },
  success: {
    backgroundColor: colors.green.primary,
    boxShadowColor: colors.green.hover,
    color: colors.white,
  },
  danger: {
    backgroundColor: colors.red.primary,
    boxShadowColor: colors.red.hover,
    color: colors.white,
  },
  warning: {
    backgroundColor: colors.orange.primary,
    boxShadowColor: colors.orange.hover,
    color: colors.white,
  },
  info: {
    backgroundColor: colors.blue.primary,
    boxShadowColor: colors.blue.hover,
    color: colors.white,
  },
  default: {
    backgroundColor: colors.yellow.primary,
    boxShadowColor: colors.black.hover,
    color: colors.white,
  },
};

// ********************************** //

const alertBox = {
  success: {
    backgroundColor: colors.green.light,
    borderColor: colors.green.primary,
    message: 'You did something awesome. Well done!',
  },
  info: {
    backgroundColor: colors.blue.light,
    borderColor: colors.blue.primary,
    message: 'You need to do something.',
  },
  warning: {
    backgroundColor: colors.orange.light,
    borderColor: colors.orange.primary,
    message: 'You are about to do something wrong.',
  },
  error: {
    backgroundColor: colors.red.light,
    borderColor: colors.red.primary,
    message: 'You did something wrong.',
  },
  default: {
    backgroundColor: colors.green.light,
    borderColor: colors.green.primary,
    message: '',
  },
};

// ********************************** //

const icon = {
  success: {
    color: colors.green.primary,
  },
  info: {
    color: colors.blue.primary,
  },
  warning: {
    color: colors.orange.primary,
  },
  error: {
    color: colors.red.primary,
  },
  // type ~ out of range
  default: {
    color: colors.black.secondary,
  },
};

// Border Radius ********************* //

type BorderRadiusSize = 'xs' | 'sm' | 'md' | 'lg' | 'xl' | 'circle';

const radiiAll = [0, 2, 4, 8, 12, 16];

/* border-radius aliases
 * xs: 2px (extra small)
 * sm: 4px (small)
 * md: 8px (medium)
 * lg: 12px (large)
 * xl: 16px (extra large)
 * circle: 1000px
 */

const radii: Record<BorderRadiusSize, number> = {
  xs: radiiAll[1],
  sm: radiiAll[2],
  md: radiiAll[3],
  lg: radiiAll[4],
  xl: radiiAll[5],
  circle: 1000,
};

// ********************************** //

type FontWeight = 'normal' | 'medium' | 'bold';

const fontWeightsAll = [0, 100, 200, 300, 400, 500, 600, 700, 800, 900];

/* font-weight aliases
 * normal: 400
 * medium: 500
 * bold: 700
 */

const fontWeights: Record<FontWeight, number> = {
  normal: fontWeightsAll[4],
  medium: fontWeightsAll[5],
  bold: fontWeightsAll[7],
};

// ********************************** //
type Element =
  | 'h1'
  | 'h2'
  | 'h3'
  | 'h4'
  | 'p'
  | 'button'
  | 'tab'
  | 'explain'
  | 'icon';

const fontSizesAll = [12, 14, 16, 18, 20, 24, 30, 36, 48, 80, 96];
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

type FontSize = Record<Element, number>;

const fontSizes: FontSize = {
  h1: fontSizesAll[6],
  h2: fontSizesAll[5],
  h3: fontSizesAll[4],
  h4: fontSizesAll[3],
  p: fontSizesAll[2],
  button: fontSizesAll[1],
  tab: fontSizesAll[3],
  explain: fontSizesAll[0],
  icon: fontSizesAll[3],
};

// ****************************** //

type Space = 'zero' | 'xs' | 'sm' | 'md' | 'lg' | 'xl';

const spacesAll = [0, 4, 6, 8, 10, 12, 14, 16, 18, 20, 32, 64];

/* space ~ margin / padding aliases
 * zero: 0
 * xs: 4px (extra small)
 * sm: 8px (small)
 * md: 16px (medium)
 * lg: 32px (large)
 * xl: 64px (extra large)
 */

const space: Record<Space, number> = {
  zero: spacesAll[0],
  xs: spacesAll[1],
  sm: spacesAll[3],
  md: spacesAll[7],
  lg: spacesAll[10],
  xl: spacesAll[11],
};

// ********************************** //

type lineHeight = 'body' | 'explain';

const lineHeightsAll = [1.33, 1.5];

/* line-height aliases
 * body: 1.5
 * explain: 1.3 ~ Explainer Text
 */

const lineHeights: Record<lineHeight, number> = {
  body: lineHeightsAll[1],
  explain: lineHeightsAll[0],
};

// ********************************** //

type Size = 'sm' | 'lg';

/* sizes aliases (width & height)
 * sm: 40px
 * lg: 48px
 */

const sizesAll = [40, 48];

const sizes: Record<Size, number> = {
  sm: sizesAll[0],
  lg: sizesAll[1],
};

// ********************************** //

export const theme = {
  colors,
  radii,
  fonts: {
    roboto: 'Roboto',
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
    '0 7px 24px 0 rgba(0, 0, 0, 0.32)',
  ],
  borders: [0, '1px solid', '2px solid', '3px solid', '4px solid', '5px solid'],
  button,
  alertBox,
  icon,
};
