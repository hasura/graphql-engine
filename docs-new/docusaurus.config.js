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
  onBrokenLinks: 'warn',
  onBrokenMarkdownLinks: 'warn',
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
            docId: 'graphql/core/actions/create',
            position: 'left',
            label: 'Core',
          },
          {
            type: 'docsVersionDropdown',
            position: 'right',
            dropdownActiveClassDisabled: true,
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
    }),
};

module.exports = config;
