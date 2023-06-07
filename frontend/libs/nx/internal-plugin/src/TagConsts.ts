import * as z from 'zod';

export type TagGroupMap = Record<
  string,
  {
    prefix: string;
    tags: readonly string[];
    description: string;
    allowMultiplePerProject: boolean;
    shouldAlwaysBePresent: boolean;
    tagsMeta: z.ZodType;
  }
>;

export type ImportMap<T extends string> = Record<T, boolean>;

export type TagDefNew<T extends TagGroupMap> = {
  [Key in keyof T]: Record<
    T[Key]['tags'][number],
    {
      description: string;
      canImport: ImportMap<T[Key]['tags'][number]>;
      meta: z.output<T[Key]['tagsMeta']>;
    }
  >;
};

export const tagGroups = {
  scope: {
    description:
      'A scope tag is used to add boundaries between modules of the monorepo.',
    prefix: 'scope:',
    tags: ['console', 'nx-plugins', 'shared'] as const,
    allowMultiplePerProject: false,
    shouldAlwaysBePresent: true,
    tagsMeta: z.object({
      libraryGenerator: z.object({
        canBeGenerated: z.boolean(),
      }),
    }),
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
    tagsMeta: z.object({
      libraryGenerator: z.object({
        canBeGenerated: z.boolean(),
        shouldGenerateStorybook: z.boolean(),
      }),
    }),
  },
  meta: {
    description: 'A meta tags is used to add artifical boundaries when needed',
    prefix: 'meta:',
    tags: ['legacy', 'package'] as const,
    allowMultiplePerProject: true,
    shouldAlwaysBePresent: false,
    tagsMeta: z.object({}),
  },
} as const satisfies TagGroupMap;

export const tagDefs = {
  scope: {
    shared: {
      description: 'Shared libraries, used across all other libraries',
      canImport: {
        shared: true,
        console: false,
        'nx-plugins': false,
      },
      meta: {
        libraryGenerator: {
          canBeGenerated: true,
        },
      },
    },
    console: {
      description: 'Console application related libraries',
      canImport: {
        console: true,
        'nx-plugins': false,
        shared: true,
      },
      meta: {
        libraryGenerator: {
          canBeGenerated: true,
        },
      },
    },
    'nx-plugins': {
      description: 'Nx plugins, internal or external',
      canImport: {
        shared: true,
        'nx-plugins': true,
        console: false,
      },
      meta: {
        libraryGenerator: {
          canBeGenerated: false,
        },
      },
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
      meta: {
        libraryGenerator: {
          canBeGenerated: true,
          shouldGenerateStorybook: false,
        },
      },
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
      meta: {
        libraryGenerator: {
          canBeGenerated: true,
          shouldGenerateStorybook: false,
        },
      },
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
      meta: {
        libraryGenerator: {
          canBeGenerated: true,
          shouldGenerateStorybook: true,
        },
      },
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
      meta: {
        libraryGenerator: {
          canBeGenerated: true,
          shouldGenerateStorybook: true,
        },
      },
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
      meta: {
        libraryGenerator: {
          canBeGenerated: false,
          shouldGenerateStorybook: false,
        },
      },
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
      meta: {
        libraryGenerator: {
          canBeGenerated: false,
          shouldGenerateStorybook: false,
        },
      },
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
      meta: {
        libraryGenerator: {
          canBeGenerated: false,
          shouldGenerateStorybook: false,
        },
      },
    },
  },
  meta: {
    legacy: {
      description:
        'A library marked as legacy is used to clearly separate the old and new patterns',
      meta: {},
      canImport: {
        legacy: true,
        package: false,
      },
    },
    package: {
      description:
        'A package is a library published to NPM. And it can only depends on publishable packages to be published',
      meta: {},
      canImport: {
        package: true,
        legacy: false,
      },
    },
  },
} as const satisfies TagDefNew<typeof tagGroups>;

export type ScopeTagsList = (typeof tagGroups)['scope']['tags'][number];
export type TypeTagsList = (typeof tagGroups)['type']['tags'][number];
