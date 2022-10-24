const commonThemeProperties = {
  fontBase:
    '"IBM Plex Sans",system-ui,-apple-system,Segoe UI,Roboto,Ubuntu,Cantarell,Noto Sans,sans-serif,BlinkMacSystemFont,"Segoe UI",Helvetica,Arial,sans-serif,"Apple Color Emoji","Segoe UI Emoji","Segoe UI Symbol"',
  fontCode:
    '"IBM Plex Mono Regular","IBM Plex Mono",SFMono-Regular,Menlo,Monaco,Consolas,"Liberation Mono","Courier New",monospace',

  brandTitle: 'Hasura',
  brandUrl: 'http://hasura.io',
  brandTarget: '_self',
};

// Color theme reversed engineered from Hasura docs (https://hasura.io/docs/latest/graphql/core/index/)
export default {
  light: {
    ...commonThemeProperties,

    base: 'light',

    colorPrimary: '#1599e2',
    colorSecondary: '#1599e2',

    // UI
    appBg: '#f4f5f7',
    appContentBg: 'white',
    appBorderColor: '#dadde1',
    appBorderRadius: 4,

    // Text colors
    textColor: '#344658',
    textInverseColor: '#cbb9a7',
    textMutedColor: '#8a929b',

    // Toolbar default and active colors
    barTextColor: '#141c22',
    barSelectedColor: '#1699e2',
    barBg: 'white',

    // Form colors
    inputBg: 'white',
    inputBorder: '#dadde1',
    inputTextColor: '#344658',
    inputBorderRadius: 4,

    brandImage: './images/hasura-storybook-dark.svg',
  },
  dark: {
    ...commonThemeProperties,

    base: 'dark',

    colorPrimary: '#1599e2',
    colorSecondary: '#1599e2',

    // UI
    appBg: '#23303d',
    appContentBg: '#131c22',
    appBorderColor: '#616770',
    appBorderRadius: 4,

    // Text colors
    textColor: '#dce2e8',
    textInverseColor: '#231d17',
    textMutedColor: '#92979c',

    // Toolbar default and active colors
    barTextColor: '#dce2e8',
    barSelectedColor: '#dce2e8',
    barBg: '#131c22',

    // Form colors
    inputBg: '#23303d',
    inputBorder: '#616770',
    inputTextColor: '#dce2e8',
    inputBorderRadius: 4,

    brandImage: './images/hasura-storybook-light.svg',
  },
};
