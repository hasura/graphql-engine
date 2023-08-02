const colors = require('tailwindcss/colors');
const plugin = require('tailwindcss/plugin');

function hexToHsl(hex) {
  // Remove the '#' character from the beginning of the hex code
  hex = hex.replace('#', '');

  // Convert hex code to RGB
  const red = parseInt(hex.substring(0, 2), 16) / 255;
  const green = parseInt(hex.substring(2, 4), 16) / 255;
  const blue = parseInt(hex.substring(4, 6), 16) / 255;

  const max = Math.max(red, green, blue);
  const min = Math.min(red, green, blue);
  let hue,
    saturation,
    lightness = (max + min) / 2;

  if (max === min) {
    hue = saturation = 0; // achromatic
  } else {
    const delta = max - min;
    saturation =
      lightness > 0.5 ? delta / (2 - max - min) : delta / (max + min);
    switch (max) {
      case red:
        hue = (green - blue) / delta + (green < blue ? 6 : 0);
        break;
      case green:
        hue = (blue - red) / delta + 2;
        break;
      case blue:
        hue = (red - green) / delta + 4;
        break;
    }
    hue *= 60;
  }

  return `${parseFloat(hue.toFixed(2))}, ${parseFloat(
    (100 * saturation).toFixed(2)
  )}%, ${parseFloat((100 * lightness).toFixed(2))}%`;
}

function dataStateVariant(state, { addVariant, e }) {
  addVariant(`data-state-${state}`, ({ modifySelectors, separator }) => {
    modifySelectors(({ className }) => {
      return `.${e(
        `data-state-${state}${separator}${className}`
      )}[data-state='${state}']`;
    });
  });

  addVariant(`group-data-state-${state}`, ({ modifySelectors, separator }) => {
    modifySelectors(({ className }) => {
      return `.group[data-state='${state}'] .${e(
        `group-data-state-${state}${separator}${className}`
      )}`;
    });
  });

  addVariant(`peer-data-state-${state}`, ({ modifySelectors, separator }) => {
    modifySelectors(({ className }) => {
      return `.peer[data-state='${state}'] ~ .${e(
        `peer-data-state-${state}${separator}${className}`
      )}`;
    });
  });
}

module.exports = {
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
      boxShadow: {
        eq: 'rgb(0 0 0) 0px 0px 2px',
      },
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
          DEFAULT: '#64748B',
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
        notificationOpen: {
          from: {
            opacity: `0`,
            transform: `translateX(100%)`,
          },
          to: {
            opacity: `1`,
            transform: `translateX(0)`,
          },
        },
        notificationClose: {
          from: {
            opacity: `1`,
            transform: `translateY(0)`,
          },
          to: {
            opacity: `0`,
            transform: `translateY(-100%)`,
          },
        },
        dropdownMenuContentOpen: {
          from: {
            opacity: '0',
          },
          to: {
            opacity: '1',
          },
        },
        dropdownMenuContentClose: {
          from: {
            opacity: '1',
          },
          to: {
            opacity: '0',
          },
        },
        fadeIn: {
          from: { opacity: 0 },
          to: { opacity: 1 },
        },
        alertContentShow: {
          from: { opacity: 0, transform: 'translate(-50%, -48%) scale(0.96)' },
          to: { opacity: 1, transform: 'translate(-50%, -50%) scale(1)' },
        },
      },
      animation: {
        collapsibleContentOpen: 'collapsibleContentOpen 300ms ease-out',
        collapsibleContentClose: 'collapsibleContentClose 300ms ease-out',
        collapsibleContentOpenFast: 'collapsibleContentOpen 200ms ease-out',
        collapsibleContentCloseFast: 'collapsibleContentClose 200ms ease-out',
        slideUpAndFade: 'slideUpAndFade 400ms cubic-bezier(0.16, 1, 0.3, 1)',
        slideRightAndFade:
          'slideRightAndFade 400ms cubic-bezier(0.16, 1, 0.3, 1)',
        slideDownAndFade:
          'slideDownAndFade 400ms cubic-bezier(0.16, 1, 0.3, 1)',
        slideLeftAndFade:
          'slideLeftAndFade 400ms cubic-bezier(0.16, 1, 0.3, 1)',
        notificationOpen: 'notificationOpen 300ms ease-in-out',
        notificationClose: 'notificationClose 300ms ease-in-out',
        dropdownMenuContentOpen: 'dropdownMenuContentOpen 100ms ease-in',
        dropdownMenuContentClose: 'dropdownMenuContentClose 100ms ease-out',
        overlayShow: 'overlayShow 150ms cubic-bezier(0.16, 1, 0.3, 1)',
        contentShow: 'contentShow 150ms cubic-bezier(0.16, 1, 0.3, 1)',
      },
    },
  },
  plugins: [
    require('@tailwindcss/typography'),
    require('@tailwindcss/forms'),
    require('tailwindcss-radix')(),
    plugin(helpers => {
      // variants that help styling Radix-UI components
      dataStateVariant('open', helpers);
      dataStateVariant('closed', helpers);
      dataStateVariant('on', helpers);
      dataStateVariant('checked', helpers);
      dataStateVariant('unchecked', helpers);
    }),
    plugin(function ({ addBase, theme }) {
      const colors = {
        '--tw-font-family-sans': theme('fontFamily.sans'),
        '--tw-font-family-mono': theme('fontFamily.mono'),
        '--tw-color-white-hex': '#ffffff',
        '--tw-color-white-hsl': '0, 0%, 100%',
        '--tw-color-black-hex': '#000000',
        '--tw-color-black-hsl': '0, 0%, 0%',
      };
      Object.entries(theme('colors'))
        .filter(([name]) => !['white', 'black'].includes(name))
        .forEach(([name, color]) => {
          Object.entries(color).forEach(([shade, value]) => {
            if (typeof value === 'object') {
              Object.entries(value).forEach(([shade, hex]) => {
                if (hex?.startsWith('#')) {
                  colors[`--tw-color-${name}-${shade}-hex`] = hex;
                  colors[`--tw-color-${name}-${shade}-hsl`] = hexToHsl(hex);
                }
              });
            } else if (typeof value === 'string' && value?.startsWith('#')) {
              colors[`--tw-color-${name}-${shade}-hex`] = value;
              colors[`--tw-color-${name}-${shade}-hsl`] = hexToHsl(value);
            }
          });
        });
      addBase({
        '*, ::before, ::after': colors,
      });
    }),
  ],
};
