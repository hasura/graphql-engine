// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const path = require('path');
const lightCodeTheme = require('prism-react-renderer/themes/vsLight');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'Hasura GraphQL Docs',
  tagline: 'Instant GraphQL on all your data',
  url: 'https://hasura.io',
  baseUrl: '/docs/',
  trailingSlash: true,
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'throw',
  favicon: '/docs/img/favicon.png',
  organizationName: 'hasura',
  projectName: 'graphql-engine',
  staticDirectories: ['static', 'public'],

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          routeBasePath: '/',
          sidebarPath: require.resolve('./sidebars.js'),
          editUrl: ({ docPath }) => `https://github.com/hasura/graphql-engine/edit/master/docs/docs/${docPath}`,
          docItemComponent: require.resolve('./src/components/CustomDocItem/index.tsx'),
          exclude: ['**/*.wip'],
          breadcrumbs: true,
          // showLastUpdateAuthor: true,
          // showLastUpdateTime: true,
          lastVersion: 'current',
          versions: {
            current: {
              label: 'v2.x',
              badge: true,
              path: 'latest',
            },
          },
        },
        theme: {
          customCss: require.resolve('./src/css/custom.scss'),
        },
      }),
    ],
  ],
  plugins: [
    'docusaurus-plugin-sass',
    [
      'content-docs',
      /** @type {import('@docusaurus/plugin-content-docs').Options} */
      ({
        id: 'wiki',
        path: 'wiki',
        routeBasePath: 'wiki',
        editUrl: ({ docPath }) => `https://github.com/hasura/graphql-engine/edit/master/docs/docs/${docPath}`,
        editCurrentVersion: true,
        docItemComponent: require.resolve('./src/components/CustomDocItem/CustomDocItemWiki.tsx'),
        // disableVersioning: true,
        breadcrumbs: false,
        sidebarPath: require.resolve('./sidebarsWiki.js'),
        showLastUpdateAuthor: true,
        showLastUpdateTime: true,
      }),
    ],
    [
      path.resolve(__dirname, './src/plugins/docusaurus-plugin-segment-analytics'),
      {
        prodKey: 'RQXoHRpNcmBKllUDihjDjupGv4AHn5TB',
        devKey: 'FRKElp5cyMax6GAdM8OVyNMIFVppgEgp',
        // boolean (defaults to false) on whether you want
        // to include analytics.page() automatically
        trackPage: true,
        // number (defaults to 50); time to wait after a route update before it should
        // track the page change, to implement this, make sure your `trackPage` property is set to `true`
        // trackPageDelay: 50,
      },
    ],
    [
      path.resolve(__dirname, './src/plugins/docusaurus-plugin-google-gtm'),
      {
        trackingID: 'GTM-PF5MQ2Z',
      },
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      docs: {
        sidebar: {
          hideable: true,
          autoCollapseCategories: true
        }
      },
      colorMode: {
        defaultMode: 'light',
        disableSwitch: false,
        respectPrefersColorScheme: true,
      },
      image: 'https://graphql-engine-cdn.hasura.io/assets/hge-docs/og-image.png',
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        additionalLanguages: ['rest', 'http', 'haskell', 'plsql', 'docker', 'nginx', 'markdown'],
      },
      algolia: {
        // If Algolia did not provide you any appId, use 'BH4D9OD16A'
        appId: 'NS6GBGYACO',
        // Public API key: it is safe to commit it
        apiKey: '8f0f11e3241b59574c5dd32af09acdc8',
        indexName: 'hasura-graphql',
        // Optional: see doc section below
        // contextualSearch: true,
        // Optional: Specify domains where the navigation should occur through window.location instead on history.push. Useful when our Algolia config crawls multiple documentation sites and we want to navigate with window.location.href to them.
        // externalUrlRegex: 'external\\.com|domain\\.com',
        // Optional: Algolia search parameters
        // searchParameters: {},
      },
      // announcementBar: {
      //   id: 'announcementBar-3', // Increment on change
      //   content: `⭐️ If you like Docusaurus, give it a star on <a target="_blank" rel="noopener noreferrer" href="https://github.com/facebook/docusaurus">GitHub</a> and follow us on <a target="_blank" rel="noopener noreferrer" href="https://twitter.com/docusaurus" >Twitter</a> ${TwitterSvg}`,
      // },
      // announcementBar: {
      //   id: 'announcement-bar-3',
      //   content:
      //     '<a target="_blank" rel="noopener noreferrer" href="https://hasura.io/events/hasura-con-2022/">Check out the product announcements from HasuraCon’22</a>',
      //   backgroundColor: '#511AAA',
      //   textColor: '#fff',
      // },
      navbar: {
        hideOnScroll: false,
        title: '',
        logo: {
          alt: 'Hasura Logo',
          src: '/img/logo.svg',
          srcDark: '/img/logo-light.svg',
          href: 'https://hasura.io',
        },
        items: [
          {
            to: 'https://hasura.io/products/',
            label: 'Product',
            position: 'left',
          },
          {
            to: 'https://hasura.io/blog/',
            label: 'Blog',
            position: 'left',
          },
          {
            to: 'https://hasura.io/learn/',
            label: 'Tutorials',
            position: 'left',
          },
          {
            to: 'https://hasura.io/changelog',
            label: "What's New",
            position: 'left',
          },
          {
            type: 'docsVersionDropdown',
            position: 'right',
            dropdownActiveClassDisabled: true,
            dropdownItemsAfter: [
              {
                href: 'https://hasura.io/docs/1.0/graphql/core/index.html',
                label: 'v1.x',
              },
            ],
          },
          {
            type: 'search',
            position: 'right',
          },
          {
            href: 'https://github.com/hasura/graphql-engine',
            position: 'right',
            className: 'header-github-link',
            'aria-label': 'GitHub repository',
          },
          {
            to: 'https://hasura.io/pricing/',
            label: 'Pricing',
            position: 'right',
          },
          {
            to: 'https://cloud.hasura.io/login?pg=docs&plcmt=header&cta=log-in&tech=default',
            label: 'Login',
            position: 'right',
            className: 'nav-link_login',
          },
          {
            to: 'https://cloud.hasura.io/signup?pg=products&plcmt=header&cta=try-hasura&tech=default',
            label: 'Get Started',
            position: 'right',
            className: 'nav-link_getting-started',
          },
        ],
      },
    }),
};

module.exports = config;
