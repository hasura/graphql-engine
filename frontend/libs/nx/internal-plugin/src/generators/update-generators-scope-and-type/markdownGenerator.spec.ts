import {
  generateDepConstrainMdTable,
  generateFullMdDocAboutTags,
  generateMarkdownDocumentationForTags,
} from './markdownGenerator';
import { TagDefNew, TagGroupMap } from '../../TagConsts';
import { z } from 'zod';

const MOCK_URL = './docs/can-import-icon.png';

const tagGroups = {
  scope: {
    description:
      'A scope tag is used to add boundaries between modules of the monorepo.',
    prefix: 'scope:',
    tags: ['dash', 'nx-plugins', 'shared'] as const,
    allowMultiplePerProject: false,
    shouldAlwaysBePresent: true,
    tagsMeta: z.any(),
  },
  type: {
    description: 'A type tag is used to add information about a library.',
    prefix: 'type:',
    tags: [
      'utils',
      'data',
      'ui',
      'feature',
      'app',
      'storybook',
      'e2e',
    ] as const,
    allowMultiplePerProject: false,
    shouldAlwaysBePresent: true,
    tagsMeta: z.any(),
  },
} as const satisfies TagGroupMap;

const tagDefs = {
  scope: {
    shared: {
      description: 'Shared libraries, used across all other libraries',
      canImport: {
        shared: true,
        dash: false,
        'nx-plugins': false,
      },
      meta: false,
    },
    dash: {
      description: 'Dash application related libraries',
      canImport: {
        dash: true,
        'nx-plugins': false,
        shared: true,
      },
      meta: false,
    },
    'nx-plugins': {
      description: 'Nx plugins, internal or external',
      canImport: {
        shared: true,
        'nx-plugins': true,
        dash: false,
      },
      meta: false,
    },
  },
  type: {
    utils: {
      description: 'Utilities library, ex : date-utils, zod-utils',
      canImport: {
        data: false,
        feature: false,
        ui: false,
        utils: true,
        e2e: false,
        app: false,
        storybook: false,
      },
      meta: false,
    },
    data: {
      description: 'Data library, with state management and network',
      canImport: {
        data: true,
        feature: false,
        ui: false,
        utils: true,
        e2e: false,
        app: false,
        storybook: false,
      },
      meta: false,
    },
    ui: {
      description: 'Ui library, with only component getting data via props',
      canImport: {
        data: false,
        feature: false,
        ui: true,
        utils: true,
        e2e: false,
        app: false,
        storybook: false,
      },
      meta: false,
    },
    feature: {
      description:
        'Feature library, with connected ui & data to make a feature',
      canImport: {
        data: true,
        feature: true,
        ui: true,
        utils: true,
        e2e: false,
        app: false,
        storybook: false,
      },
      meta: false,
    },
    app: {
      description: 'Application, where the orchestration is done',
      canImport: {
        data: true,
        app: false,
        utils: true,
        ui: true,
        e2e: false,
        feature: true,
        storybook: false,
      },
      meta: false,
    },
    storybook: {
      description: 'A storybook host, used to aggregate stories',
      canImport: {
        data: true,
        e2e: false,
        app: false,
        storybook: true,
        utils: true,
        feature: true,
        ui: true,
      },
      meta: false,
    },
    e2e: {
      description: 'End to end tests projects',
      canImport: {
        data: true,
        e2e: true,
        app: true,
        ui: true,
        storybook: true,
        feature: true,
        utils: true,
      },
      meta: false,
    },
  },
} as const satisfies TagDefNew<typeof tagGroups>;

describe('generateDepConstrainMdTable', () => {
  it('should generate the table correctly', () => {
    const result = generateDepConstrainMdTable(
      {
        first: {
          description: 'This is my first',
          canImport: {
            first: true,
            second: false,
          },
          meta: false,
        },
        second: {
          description: 'This is my second',
          canImport: {
            first: true,
            second: true,
          },
          meta: false,
        },
      },
      'scope:',
      MOCK_URL
    );

    expect(result).toMatchInlineSnapshot(`
      "| ![Can row import colum](./docs/can-import-icon.png) | \`scope:first\` | \`scope:second\` |
      |---|:---:|:---:|
      |\`scope:first\` |  ✅ | ⛔ |
      |\`scope:second\` |  ✅ | ✅ |"
    `);
  });
  it('should generate the table correctly even if the order in canImport differnt', () => {
    const result = generateDepConstrainMdTable(
      {
        first: {
          description: 'This is my first',
          canImport: {
            second: false,
            first: true,
          },
          meta: false,
        },
        second: {
          description: 'This is my second',
          canImport: {
            first: true,
            second: true,
          },
          meta: false,
        },
      },
      'scope:',
      MOCK_URL
    );

    expect(result).toMatchInlineSnapshot(`
      "| ![Can row import colum](./docs/can-import-icon.png) | \`scope:first\` | \`scope:second\` |
      |---|:---:|:---:|
      |\`scope:first\` |  ✅ | ⛔ |
      |\`scope:second\` |  ✅ | ✅ |"
    `);
  });
  it('should work with a huge map', () => {
    const result = generateDepConstrainMdTable(
      {
        utils: {
          description: 'utils libs',
          canImport: {
            data: false,
            feature: false,
            ui: false,
            utils: true,
            e2e: false,
            app: false,
            storybook: false,
          },
          meta: false,
        },
        data: {
          description: 'data libs',
          canImport: {
            data: true,
            feature: false,
            ui: false,
            utils: true,
            e2e: false,
            app: false,
            storybook: false,
          },
          meta: false,
        },
        ui: {
          description: 'Ui libs',
          canImport: {
            data: false,
            feature: false,
            ui: true,
            utils: true,
            e2e: false,
            app: false,
            storybook: false,
          },
          meta: false,
        },
        feature: {
          description: 'feature libs',
          canImport: {
            data: true,
            feature: true,
            ui: true,
            utils: true,
            e2e: false,
            app: false,
            storybook: false,
          },
          meta: false,
        },
        app: {
          description: 'Applications',
          canImport: {
            data: true,
            app: false,
            utils: true,
            ui: true,
            e2e: false,
            feature: true,
            storybook: false,
          },
          meta: false,
        },
        storybook: {
          description: 'A storybook host, used to agregate stories',
          canImport: {
            data: true,
            e2e: false,
            app: false,
            storybook: true,
            utils: true,
            feature: true,
            ui: true,
          },
          meta: false,
        },
        e2e: {
          description: 'Test e2e projects',
          canImport: {
            data: true,
            e2e: true,
            app: true,
            ui: true,
            storybook: true,
            feature: true,
            utils: true,
          },
          meta: false,
        },
      },
      'scope:',
      MOCK_URL
    );

    expect(result).toMatchInlineSnapshot(`
      "| ![Can row import colum](./docs/can-import-icon.png) | \`scope:utils\` | \`scope:data\` | \`scope:ui\` | \`scope:feature\` | \`scope:app\` | \`scope:storybook\` | \`scope:e2e\` |
      |---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
      |\`scope:utils\` |  ✅ | ⛔ | ⛔ | ⛔ | ⛔ | ⛔ | ⛔ |
      |\`scope:data\` |  ✅ | ✅ | ⛔ | ⛔ | ⛔ | ⛔ | ⛔ |
      |\`scope:ui\` |  ✅ | ⛔ | ✅ | ⛔ | ⛔ | ⛔ | ⛔ |
      |\`scope:feature\` |  ✅ | ✅ | ✅ | ✅ | ⛔ | ⛔ | ⛔ |
      |\`scope:app\` |  ✅ | ✅ | ✅ | ✅ | ⛔ | ⛔ | ⛔ |
      |\`scope:storybook\` |  ✅ | ✅ | ✅ | ✅ | ⛔ | ✅ | ⛔ |
      |\`scope:e2e\` |  ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |"
    `);
  });
});

describe('generateMarkdownDocumentationForTags', () => {
  it('should output the full content', () => {
    const output = generateMarkdownDocumentationForTags(
      tagGroups,
      tagDefs,
      MOCK_URL
    );

    expect(output).toMatchInlineSnapshot(`
      "## \`scope\` tag group

      A scope tag is used to add boundaries between modules of the monorepo.

      ### Tag list :

      #### \`scope:dash\`

      Dash application related libraries


      #### \`scope:nx-plugins\`

      Nx plugins, internal or external


      #### \`scope:shared\`

      Shared libraries, used across all other libraries


      ### Constraints

      - There can be only one \`scope\` tag per library.

      - A \`scope\` tag should be present on all libraries.

      Here is the import rule matrix :

      | ![Can row import colum](./docs/can-import-icon.png) | \`scope:shared\` | \`scope:dash\` | \`scope:nx-plugins\` |
      |---|:---:|:---:|:---:|
      |\`scope:shared\` |  ✅ | ⛔ | ⛔ |
      |\`scope:dash\` |  ✅ | ✅ | ⛔ |
      |\`scope:nx-plugins\` |  ✅ | ⛔ | ✅ |
            

      ## \`type\` tag group

      A type tag is used to add information about a library.

      ### Tag list :

      #### \`type:utils\`

      Utilities library, ex : date-utils, zod-utils


      #### \`type:data\`

      Data library, with state management and network


      #### \`type:ui\`

      Ui library, with only component getting data via props


      #### \`type:feature\`

      Feature library, with connected ui & data to make a feature


      #### \`type:app\`

      Application, where the orchestration is done


      #### \`type:storybook\`

      A storybook host, used to aggregate stories


      #### \`type:e2e\`

      End to end tests projects


      ### Constraints

      - There can be only one \`type\` tag per library.

      - A \`type\` tag should be present on all libraries.

      Here is the import rule matrix :

      | ![Can row import colum](./docs/can-import-icon.png) | \`type:utils\` | \`type:data\` | \`type:ui\` | \`type:feature\` | \`type:app\` | \`type:storybook\` | \`type:e2e\` |
      |---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
      |\`type:utils\` |  ✅ | ⛔ | ⛔ | ⛔ | ⛔ | ⛔ | ⛔ |
      |\`type:data\` |  ✅ | ✅ | ⛔ | ⛔ | ⛔ | ⛔ | ⛔ |
      |\`type:ui\` |  ✅ | ⛔ | ✅ | ⛔ | ⛔ | ⛔ | ⛔ |
      |\`type:feature\` |  ✅ | ✅ | ✅ | ✅ | ⛔ | ⛔ | ⛔ |
      |\`type:app\` |  ✅ | ✅ | ✅ | ✅ | ⛔ | ⛔ | ⛔ |
      |\`type:storybook\` |  ✅ | ✅ | ✅ | ✅ | ⛔ | ✅ | ⛔ |
      |\`type:e2e\` |  ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
            "
    `);
  });
});

describe('generateFullMdDocAboutTags', () => {
  it('should output the full content', () => {
    const output = generateFullMdDocAboutTags(tagGroups, tagDefs, MOCK_URL);

    expect(output).toMatchInlineSnapshot(`
      "# Tags in the workspace and their organisation

      In this workspace, we use tags to organise the libraries and application to ensure have strong boundaries between projects.

      There is 2 tag groups : scope, type.

      You should use the internal generators to create libraries and application, they will have selectors for the tags groups.

      Next here, you can find all of the tags that we have, along side the explanation and the import rule matrix.

      ## \`scope\` tag group

      A scope tag is used to add boundaries between modules of the monorepo.

      ### Tag list :

      #### \`scope:dash\`

      Dash application related libraries


      #### \`scope:nx-plugins\`

      Nx plugins, internal or external


      #### \`scope:shared\`

      Shared libraries, used across all other libraries


      ### Constraints

      - There can be only one \`scope\` tag per library.

      - A \`scope\` tag should be present on all libraries.

      Here is the import rule matrix :

      | ![Can row import colum](./docs/can-import-icon.png) | \`scope:shared\` | \`scope:dash\` | \`scope:nx-plugins\` |
      |---|:---:|:---:|:---:|
      |\`scope:shared\` |  ✅ | ⛔ | ⛔ |
      |\`scope:dash\` |  ✅ | ✅ | ⛔ |
      |\`scope:nx-plugins\` |  ✅ | ⛔ | ✅ |
            

      ## \`type\` tag group

      A type tag is used to add information about a library.

      ### Tag list :

      #### \`type:utils\`

      Utilities library, ex : date-utils, zod-utils


      #### \`type:data\`

      Data library, with state management and network


      #### \`type:ui\`

      Ui library, with only component getting data via props


      #### \`type:feature\`

      Feature library, with connected ui & data to make a feature


      #### \`type:app\`

      Application, where the orchestration is done


      #### \`type:storybook\`

      A storybook host, used to aggregate stories


      #### \`type:e2e\`

      End to end tests projects


      ### Constraints

      - There can be only one \`type\` tag per library.

      - A \`type\` tag should be present on all libraries.

      Here is the import rule matrix :

      | ![Can row import colum](./docs/can-import-icon.png) | \`type:utils\` | \`type:data\` | \`type:ui\` | \`type:feature\` | \`type:app\` | \`type:storybook\` | \`type:e2e\` |
      |---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
      |\`type:utils\` |  ✅ | ⛔ | ⛔ | ⛔ | ⛔ | ⛔ | ⛔ |
      |\`type:data\` |  ✅ | ✅ | ⛔ | ⛔ | ⛔ | ⛔ | ⛔ |
      |\`type:ui\` |  ✅ | ⛔ | ✅ | ⛔ | ⛔ | ⛔ | ⛔ |
      |\`type:feature\` |  ✅ | ✅ | ✅ | ✅ | ⛔ | ⛔ | ⛔ |
      |\`type:app\` |  ✅ | ✅ | ✅ | ✅ | ⛔ | ⛔ | ⛔ |
      |\`type:storybook\` |  ✅ | ✅ | ✅ | ✅ | ⛔ | ✅ | ⛔ |
      |\`type:e2e\` |  ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
            

      ## FAQ

      > How big/small a library should be ?

      A library can be as little as a single function to as big as a full page. You can find the [tradeoffs from the Nx docs](https://nx.dev/more-concepts/creating-libraries).

        "
    `);
  });
});
