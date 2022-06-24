const colors = require('tailwindcss/colors');

module.exports = {
  purge: ['./src/**/*.{js,jsx,ts,tsx,html,md,mdx}'],
  darkMode: false, // or 'media' or 'class'
  // jit: true,
  mode: 'jit',
  theme: {
    extend: {
      colors: {
        current: 'currentColor',
        yellow: colors.amber,
        gray: colors.blueGray,
        primary: {
          light: '#fad170',
          DEFAULT: '#f9c548',
          dark: '#d5ae52',
          darker: '#ae8e3e',
        },
        // added this new colour from tailwindv3, can be removed on tailwind upgrade
        slate: {
          700: '#334155'
        },
        cloud: {
          DEFAULT: '#1eb4d4',
          dark: '#197b98',
          darker: '#074c4c',
        },
        secondary: {
          light: '#eef4f7',
          DEFAULT: '#297393',
          dark: '#14394a',
        },
        legacybg: {
          DEFAULT: '#f8fafb',
        },
        muted: {
          DEFAULT: '#475569',
        },
      },
      fontFamily: {
        sans: ['Gudea', 'ui-sans-serif', 'system-ui'],
      },
      spacing: {
        xs: '0.571rem', // ~9.5 px
        sm: '0.857rem', // ~14 px
        md: '1.429rem', // ~23 px
        lg: '2.286rem', // 36.5px
        xl: '2.857rem', // 45.7px
        input: '2.5rem', // 41.5px
        btn: '2.5rem', // 41.5px
        btnsm: '2rem', // 32px
        formlabel: '0.571rem', // ~9.5 px
      },
      fontSize: {
        sm: ['0.875rem', { lineHeight: '1.25rem' }],
        base: ['1rem', { lineHeight: '1.5rem' }],
        lg: ['1.143rem', { lineHeight: '1.429rem' }],
        xl: ['1.714rem', { lineHeight: '2.143rem' }],
        subtitle: ['1.300rem', { lineHeight: '2.143rem' }], // ~20 px
        title: ['1.5rem', { lineHeight: '2.143rem' }], // ~24 px
      },
    },
  },
  plugins: [require('@tailwindcss/typography'), require('@tailwindcss/forms')],
};
