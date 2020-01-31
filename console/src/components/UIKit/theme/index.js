// Console ~ Theme object

export const theme = {
  colors: {
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

    black: {
      original: '#000',
      secondary: '#484538',
      text: '#292822',
    },

    white: '#fff',

    transparent: 'transparent',
  },

  fonts: {
    roboto: 'Roboto',
  },

  fontWeights: [0, 100, 200, 300, 400, 500, 600, 700, 800, 900],

  fontSizes: [12, 14, 16, 18, 20, 24, 30, 36, 48, 80, 96],

  // width & height
  sizes: [40, 48],

  // margins & paddings
  space: [0, 4, 6, 8, 10, 12, 14, 16, 18, 20, 32, 64],

  // Text / box-shadows
  shadows: [
    0,
    '0 0 3px 0 rgba(0, 0, 0, 0.16)',
    '0 3px 6px 0 rgba(0, 0, 0, 0.16)',
    '0 3px 10px 0 rgba(0, 0, 0, 0.16)',
    '0 7px 24px 0 rgba(0, 0, 0, 0.32)',
  ],

  transition: {
    true: 'all .2s ease-out',
  },

  // border
  borders: [0, '1px solid', '2px solid', '1px solid', '4px solid', '5px solid'],

  // border-radius values
  radii: [0, 2, 4, 8, 12, 16],

  // line-height values
  lineHeights: [1.33, 1.5],
};

// aliases **************************** //

/* border-radius
 * xs: 2px (extra small)
 * sm: 4px (small)
 * md: 8px (medium)
 * lg: 12px (large)
 * xl: 16px (extra large)
 * circle: 1000px
 */

theme.radii.xs = theme.radii[1];

theme.radii.sm = theme.radii[2];

theme.radii.md = theme.radii[3];

theme.radii.lg = theme.radii[4];

theme.radii.xl = theme.radii[5];

theme.radii.circle = 1000;

// ********************************* //

/* font-weight
 * normal: 400
 * medium: 500
 * bold: 700
 */

theme.fontWeights.normal = theme.fontWeights[4];

theme.fontWeights.medium = theme.fontWeights[5];

theme.fontWeights.bold = theme.fontWeights[7];

// ********************************* //

/* font-sizes
 * h1: 30px
 * h2: 24px
 * h3: 20px
 * h4: 18px
 * p: 16px
 * button: 14px
 * explain (Explainer Text): 12px
 */

theme.fontSizes.h1 = theme.fontSizes[6];

theme.fontSizes.h2 = theme.fontSizes[5];

theme.fontSizes.h3 = theme.fontSizes[4];

theme.fontSizes.h4 = theme.fontSizes[3];

theme.fontSizes.p = theme.fontSizes[2];

theme.fontSizes.button = theme.fontSizes[1];

theme.fontSizes.explain = theme.fontSizes[0];

// ********************************* //

/* space ~ margin / padding
 * xs: 4px (extra small)
 * sm: 8px (small)
 * md: 16px (medium)
 * lg: 32px (large)
 * xl: 64px (extra large)
 */

theme.space.xs = theme.space[1];

theme.space.sm = theme.space[3];

theme.space.md = theme.space[7];

theme.space.lg = theme.space[10];

theme.space.xl = theme.space[11];

// ********************************* //

/* line-height
 * body: 1.5
 * explain: 1.3 ~ Explainer Text
 */

theme.lineHeights.body = theme.lineHeights[1];

theme.lineHeights.explain = theme.lineHeights[0];
