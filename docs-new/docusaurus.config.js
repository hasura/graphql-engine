// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'Hasura GraphQL Docs',
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
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          routeBasePath: "/",
          sidebarPath: require.resolve('./sidebars.js'),
          editUrl: 'https://github.com/hasura/graphql-engine/edit/main/docs-new/',
          // docItemComponent: require.resolve('./src/components/CustomDocLayout/CustomDocLayout.tsx'),
          exclude: ['**/*.wip', 'migration-guide'],
          // showLastUpdateAuthor: true,
          // showLastUpdateTime: true,
          lastVersion: "current",
          versions: {
            current: {
              label: "v2.x",
              badge: true,
              // path: 'latest'
            },
          }
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],
  // plugins: [
  //   [
  //     'ideal-image',
  //     {
  //       quality: 70,
  //       max: 1030, // max resized image's size.
  //       min: 640, // min resized image's size. if original is lower, use that size.
  //       steps: 2, // the max number of images generated between min and max (inclusive)
  //       disableInDev: false,
  //     },
  //   ],
  // ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      hideableSidebar: true,
      autoCollapseSidebarCategories: true,
      colorMode: {
        defaultMode: 'light',
        disableSwitch: false,
        respectPrefersColorScheme: true,
      },
      // announcementBar: {
      //   id: 'announcementBar-2', // Increment on change
      //   content: `⭐️ If you like Docusaurus, give it a star on <a target="_blank" rel="noopener noreferrer" href="https://github.com/facebook/docusaurus">GitHub</a> and follow us on <a target="_blank" rel="noopener noreferrer" href="https://twitter.com/docusaurus" >Twitter</a> ${TwitterSvg}`,
      // },
      navbar: {
        hideOnScroll: true,
        title: '',
        logo: {
          alt: 'Hasura Logo',
          src: '/img/logo-lightbg.svg',
          srcDark: '/img/logo.svg',
          href: 'https://hasura.io'
        },
        items: [
          {
            type: 'doc',
            position: 'left',
            docId: 'graphql/core/index',
            label: 'Hasura Core',
          },
          {
             type: 'docSidebar',
             position: 'left',
             sidebarId: 'cloudDocsSidebar',
             label: 'Hasura Cloud',
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
        style: 'light',
        links: [
          {
            title: 'Hasura',
            items: [
              {
                label: 'About Us',
                to: 'https://hasura.io/about/',
              },
              {
                label: 'Press',
                to: 'https://hasura.io/press/',
              },
              {
                label: 'Careers',
                to: 'https://hasura.io/careers/',
              },
              {
                label: 'Contact Us',
                to: 'https://hasura.io/contact-us/',
              },
              {
                label: 'Legal Stuff',
                to: 'https://hasura.io/legal/',
              },
            ],
          },
          {
            title: 'Support',
            items: [
              {
                label: 'Documentation',
                href: '/graphql/core/index',
              },
              {
                label: 'Community Forum',
                href: 'https://discordapp.com/invite/hasura',
              },
              {
                label: 'Help',
                href: 'https://hasura.io/help/',
              },
              {
                label: 'Github',
                href: 'https://github.com/hasura',
              },
              {
                label: 'Hasura Cloud Status',
                href: 'https://status.hasura.io/',
              },
            ],
          },
          {
            title: 'Tools',
            items: [
              {
                label: 'Graphqurl',
                href: 'https://github.com/hasura/graphqurl',
              },
              {
                label: 'Firebase2GraphQL',
                href: 'https://github.com/hasura/firebase2graphql',
              },
              {
                label: 'JSON2GraphQL',
                href: 'https://github.com/hasura/json2graphql',
              },
              {
                label: 'GraphQL2ChartJS',
                href: 'https://github.com/hasura/graphql2chartjs',
              },
            ],
          },
          {
            title: 'Product',
            items: [
              {
                label: 'Hasura Open Source',
                href: 'https://hasura.io/opensource/',
              },
              {
                label: 'Hasura Cloud',
                href: 'https://hasura.io/cloud/',
              },
              {
                label: 'Hasura Enterprise',
                href: 'https://hasura.io/enterprise/',
              },
              {
                label: 'Pricing',
                href: 'https://hasura.io/pricing/',
              },
              {
                label: 'Changelog',
                href: 'https://github.com/hasura/graphql-engine/releases',
              },
            ],
          },
          {
            title: 'Resources',
            items: [
              {
                label: 'Blog',
                href: 'https://hasura.io/blog/',
              },
              {
                label: 'Case Studies',
                href: 'https://hasura.io/case-studies/',
              },
              {
                label: '3Factor Apps',
                href: 'https://3factor.app/',
              },
              {
                label: 'Event Driven Programming',
                href: 'https://hasura.io/event-driven-programming/',
              },
              {
                label: 'React GraphQL',
                href: 'https://hasura.io/react-graphql/',
              },
              {
                label: 'Vue GraphQL',
                href: 'https://hasura.io/vue-graphql/',
              },
              {
                label: 'DIY GraphQL BaaS',
                href: 'https://hasura.io/diy-graphql-baas/',
              },
              {
                label: 'GraphQL & Hasura',
                href: 'https://hasura.io/graphql/',
              },
              {
                label: 'Hasura Cloud Security',
                href: 'https://hasura.io/security/',
              },
            ],
          },
          {
            title: 'Community',
            items: [
              {
                label: 'Community Resources',
                href: 'https://hasura.io/community/',
              },
              {
                label: 'GraphQL Tutorials',
                href: 'https://hasura.io/learn/',
              },
              {
                label: 'Community Wiki',
                href: 'https://github.com/hasura/graphql-engine/wiki',
              },
              {
                label: 'Sample Apps',
                href: 'https://hasura.io/sample-apps/',
              },
              {
                label: 'Partnership Program',
                href: 'https://hasura.io/partner-agencies/',
              },
              {
                label: 'HasuraCon 2021',
                href: 'https://hasura.io/events/hasura-con-2021/',
              },
              {
                label: 'GraphQL Asia',
                href: 'https://graphql.asia/',
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} Hasura Inc. All rights reserved`,
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
