// Console ~ Theme object

export const theme = {
  colors: {
    yellows: ['#ffff00', '#f8d721', 'rgba(204, 177, 25, 0.4)'],
    blacks: ['#000', '#484538', '#292822'],
    greens: ['##008000', '#69cb43', 'rgba(123, 179, 66, 0.4)', '#f0f8e7'],
    reds: ['#ff0000', '#e53935', 'rgba(229, 57, 53, 0.4)', '#f7e9e9'],
    blues: ['#0000ff', '#1f88e5', 'rgba(31, 136, 229, 0.4)', '#f0f8ff'],
    oranges: ['#ffa500', '#fdb02c', 'rgba(253, 176, 44, 0.4)', '#fff8ed'],
    white: '#fff',
    transparent: 'transparent',
  },
  fonts: {
    roboto: 'Roboto',
  },
  fontWeights: [100, 200, 300, 400, 500, 600, 700, 800, 900],
  fontSizes: [12, 14, 16, 18, 20, 24, 30, 36, 48, 80, 96],
  // border
  borders: [0, '1px solid', '2px solid', '4px solid', '5px solid'],
  // border radius
  radii: [0, 2, 4, 8, 16],
  // height
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
};
