const colors = require('tailwindcss/colors');

module.exports = {
  purge: ['./src/**/*.{js,jsx,ts,tsx,html,md,mdx}'],
  darkMode: false, // or 'media' or 'class'
  // jit: true,
  mode: 'jit',
  theme: {
    extend: {
      colors: {
        yellow: colors.amber,
        gray: colors.blueGray,
        primary: {
          light: '#fad170',
          DEFAULT: '#f9c548',
          dark: '#d5ae52',
          darker: '#ae8e3e',
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
        }
      },
      fontFamily: {
        sans: ['Gudea', 'ui-sans-serif', 'system-ui'],
      },
      spacing: {
        xs: '0.25rem',
        sm: '0.5rem',
        md: '1rem',
        lg: '2rem',
        xl: '2.75rem',
        input: '2.5rem',
        btn: '2.5rem',
        btnsm: '1.75rem',
      },
      fontSize: {
        sm: ['0.875rem', { lineHeight: '1.225rem' }],
        base: ['1rem', { lineHeight: '1.4rem' }],
        lg: ['1.25rem', { lineHeight: '1.75rem' }],
        xl: ['1.75rem', { lineHeight: '2.45rem' }],
      },
      typography: {
        DEFAULT: {
          css: {
            maxWidth: '100%',
          },
        },
      },
    },
  },
  plugins: [require('@tailwindcss/typography'), require('@tailwindcss/forms')],
};
