// Console ~ Theme object

export const theme = {
  colors: {
    red: {
      original: '#ff0000',
      active: '#e53935',
      hover: 'rgba(229, 57, 53, 0.4)',
      light: '#f7e9e9',
    },

    green: {
      original: '#008000',
      active: '#69cb43',
      hover: 'rgba(123, 179, 66, 0.4)',
      light: '#f0f8e7',
    },

    blue: {
      original: '#0000ff',
      active: '#1f88e5',
      hover: 'rgba(31, 136, 229, 0.4)',
      light: '#f0f8ff',
    },

    orange: {
      original: '#ffa500',
      active: '#fdb02c',
      hover: 'rgba(253, 176, 44, 0.4)',
      light: '#fff8ed',
    },

    yellow: {
      original: '#ffff00',
      active: '#f8d721',
      hover: 'rgba(204, 177, 25, 0.4)',
    },

    black: {
      original: '#000',
      active: '#484538',
      text: '#292822',
    },

    white: '#fff',

    transparent: 'transparent',
  },

  fonts: {
    roboto: 'Roboto',
  },

  fontWeights: [100, 200, 300, 400, 500, 600, 700, 800, 900],

  fontSizes: [12, 14, 16, 18, 20, 24, 30, 36, 48, 80, 96],

  // width & height
  sizes: [40, 48],

  // margins & paddings
  space: [0, 4, 8, 16, 32, 64],

  // Text / box-shadows
  shadows: [
    '0 0 3px 0 rgba(0, 0, 0, 0.16)',
    '0 3px 6px 0 rgba(0, 0, 0, 0.16)',
    '0 3px 10px 0 rgba(0, 0, 0, 0.16)',
    '0 7px 24px 0 rgba(0, 0, 0, 0.32)',
  ],

  transition: {
    true: 'all .2s ease-out',
  },

  // border
  borders: [0, '1px solid', '2px solid', '4px solid', '5px solid'],

  // border-radius values
  radii: [0, 2, 4, 8, 12, 16],
};

// aliases **************************** //

/* border-radius
  - xs: extra small (2px)
  - sm: small (4px)
  - md: medium (8px)
  - lg: large (12px)
  - xl: extra large (16px)
  - circle: '1000px;
*/

theme.radii.xs = theme.radii[1];

theme.radii.sm = theme.radii[2];

theme.radii.md = theme.radii[3];

theme.radii.lg = theme.radii[4];

theme.radii.xl = theme.radii[5];

theme.radii.circle = 1000;

// *********************************** //

/* font-weight
  - normal: 400
  - medium: 500
  - bold: 700
*/

theme.fontWeights.normal = theme.fontWeights[3];

theme.fontWeights.medium = theme.fontWeights[4];

theme.fontWeights.bold = theme.fontWeights[6];
