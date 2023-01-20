const colors = require('tailwindcss/colors');

module.exports = {
  content: ['./src/**/*.{js,jsx,ts,tsx,html,md,mdx}'],
  safelist: {
    standard: /background$/,
  },
  darkMode: 'class',
  theme: {
    fontFamily: {
      sans: ['Gudea', 'ui-sans-serif', 'system-ui'],
      mono: ['"Overpass Mono"', 'ui-monospace', 'monospace'],
    },
    extend: {
      colors: {
        current: 'currentColor',
        yellow: colors.amber,
        gray: colors.slate,
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
          dark: '#64748B',
        },
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
      width: {
        inherit: 'inherit',
      },
      keyframes: {
        onboardingWizardFadeIn: {
          '0%': {
            opacity: '0',
            scale: '.9',
          },
          '50%': {
            opacity: '.5',
            scale: '.95',
          },
          '100%': {
            opacity: '1',
            scale: '1',
          },
        },
        collapsibleContentOpen: {
          from: {
            height: '0',
          },
          to: {
            height: 'var(--radix-collapsible-content-height)',
          },
        },
        collapsibleContentClose: {
          from: {
            height: 'var(--radix-collapsible-content-height)',
          },
          to: {
            height: '0',
          },
        },
        slideUpAndFade: {
          from: {
            opacity: `0`,
            transform: `translateY(2px)`,
          },
          to: {
            opacity: `1`,
            transform: `translateY(0)`,
          },
        },
        slideRightAndFade: {
          from: {
            opacity: `0`,
            transform: `translateX(-2px)`,
          },
          to: {
            opacity: `1`,
            transform: `translateX(0)`,
          },
        },
        slideDownAndFade: {
          from: {
            opacity: `0`,
            transform: `translateY(-2px)`,
          },
          to: {
            opacity: `1`,
            transform: `translateY(0)`,
          },
        },
        slideLeftAndFade: {
          from: {
            opacity: `0`,
            transform: `translateX(2px)`,
          },
          to: {
            opacity: `1`,
            transform: `translateX(0)`,
          },
        },
      },
      animation: {
        collapsibleContentOpen: 'collapsibleContentOpen 300ms ease-out',
        collapsibleContentClose: 'collapsibleContentClose 300ms ease-out',
        slideUpAndFade: 'slideUpAndFade 400ms cubic-bezier(0.16, 1, 0.3, 1)',
        slideRightAndFade:
          'slideRightAndFade 400ms cubic-bezier(0.16, 1, 0.3, 1)',
        slideDownAndFade:
          'slideDownAndFade 400ms cubic-bezier(0.16, 1, 0.3, 1)',
        slideLeftAndFade:
          'slideLeftAndFade 400ms cubic-bezier(0.16, 1, 0.3, 1)',
      },
    },
  },
  plugins: [
    require('@tailwindcss/typography'),
    require('@tailwindcss/forms'),
    require('tailwindcss-radix')(),
  ],
};
