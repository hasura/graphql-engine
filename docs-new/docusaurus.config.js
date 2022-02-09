// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'Hasura GraphQL Engine Documentation | Hasura GraphQL Docs',
  tagline: 'Instant GraphQL on all your data',
  url: 'https://hasura.io/',
  baseUrl: '/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'throw',
  favicon: '/img/favicon.png',
  organizationName: 'hasura',
  projectName: 'graphql-engine',

  presets: [
    [
      '@docusaurus/preset-classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          routeBasePath: "/",
          sidebarPath: require.resolve('./sidebars.js'),
          editUrl: 'https://github.com/hasura/graphql-engine/edit/main/docs-new/',
          docItemComponent: require.resolve('./src/components/CustomDocLayout/CustomDocLayout.tsx'),
          lastVersion: "current",
          versions: {
            current: {
              label: "v2.x",
              badge: true,
            },
          }
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        title: '',
        logo: {
          alt: 'Hasura Logo',
          src: '/img/logo-lightbg.svg',
          srcDark: '/img/logo.svg',
        },
        items: [
          {
            type: 'doc',
            docId: 'graphql/cloud/index',
            position: 'left',
            label: 'Cloud',
          },
          {
            type: 'doc',
            docId: 'graphql/core/index',
            position: 'left',
            label: 'Core',
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
            href: 'https://github.com/hasura/graphql-engine',
            label: 'GitHub',
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Docs',
            items: [
              {
                label: 'Hasura GraphQL Engine',
                to: '/graphql/core/index',
              },
              {
                label: 'Hasura Cloud',
                to: '/graphql/cloud/index',
              },
              {
                label: 'Migration Guide',
                to: '/category/migration-guide',
              },
            ],
          },
          {
            title: 'Community',
            items: [
              {
                label: 'Stack Overflow',
                href: 'https://stackoverflow.com/questions/tagged/hasura',
              },
              {
                label: 'Discord',
                href: 'https://discordapp.com/invite/hasura',
              },
              {
                label: 'Twitter',
                href: 'https://twitter.com/HasuraHQ',
              },
            ],
          },
          {
            title: 'More',
            items: [
              {
                label: 'GitHub',
                href: 'https://github.com/hasura/graphql-engine',
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} Hasura, Inc. Built with Docusaurus.`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        additionalLanguages: ['rest', 'http', 'haskell', 'plsql', 'docker']
      },
      algolia: {
        // If Algolia did not provide you any appId, use 'BH4D9OD16A'
        appId: 'WCBB1VVLRC',
        // Public API key: it is safe to commit it
        apiKey: 'd1a0464a2205f31ec8eb34d05bc95d5f',
        indexName: 'dev_docs-modernization',
        // Optional: see doc section below
        // contextualSearch: true,
        // Optional: Specify domains where the navigation should occur through window.location instead on history.push. Useful when our Algolia config crawls multiple documentation sites and we want to navigate with window.location.href to them.
        // externalUrlRegex: 'external\\.com|domain\\.com',
        // Optional: Algolia search parameters
        // searchParameters: {},
      },
    }),
};

module.exports = config;
