import dedent from 'dedent';
import { Meta, StoryObj } from '@storybook/react-webpack5';
import { RestEndpointForm } from './RestEndpointForm';

export default {
  title: 'Features/REST endpoints/Form ✨',
  component: RestEndpointForm,
  parameters: {
    docs: { source: { type: 'code' } },
    chromatic: { disableSnapshot: true },
  },
} as Meta<typeof RestEndpointForm>;

const Template = (args: any) => <RestEndpointForm {...args} />;

const TemplateStoriesFactory =
  (
    Template: (stories: Record<string, any>) => React.ReactNode,
    classNames = '',
  ) =>
  (stories: Record<string, any>): React.ReactNode => (
    <div className="w-full">
      {Object.entries(stories)
        // Only use objects as function are events handlers injected by storybook
        .filter(
          ([, story]) =>
            typeof story === 'object' && !story.disableSnapshotTesting,
        )
        .map(([storyName, story]) => (
          <div key={storyName}>
            <div className="text-black dark:text-white bg-gray-100 dark:bg-gray-700 underline p-2">
              {storyName}
            </div>
            <div className={`py-4 ${classNames}`}>{Template(story)}</div>
          </div>
        ))}
    </div>
  );

export const stories = {
  'Demo - Creation form': {
    formState: {
      request: dedent`query Catalog {
          catalog {
            id
            name
            description
          }
        }`,
    },
  },
  'Demo - Edition form': {
    mode: 'edit',
    formState: {
      name: 'The endpoint name',
      comment: 'The endpoint description',
      url: 'location',
      methods: ['GET', 'PATCH'],
      request: dedent`query Catalog {
          catalog {
            id
            name
            description
          }
        }`,
    },
  },
  'State - Loading form': {
    mode: 'edit',
    formState: {
      name: 'The endpoint name',
      comment: 'The endpoint description',
      url: 'location',
      methods: ['GET', 'PATCH'],
      request: dedent`query Catalog {
          catalog {
            id
            name
            description
          }
        }`,
    },
    loading: true,
  },
  'API playground': {
    disableSnapshotTesting: true,
    formState: {
      request: dedent`query Catalog {
          catalog {
            id
            name
            description
          }
        }`,
    },
  },
};

export const CreationForm: StoryObj = {
  name: 'Creation form',
  args: stories['Demo - Creation form'],
  render: (args) => Template(args),
};

export const EditionForm: StoryObj = {
  name: 'Edition form',
  args: stories['Demo - Edition form'],
  render: (args) => Template(args),
};

export const LoadingForm: StoryObj = {
  name: 'Loading form',
  args: stories['State - Loading form'],
  render: (args) => Template(args),
};

export const ApiPlayground: StoryObj = {
  name: 'API playground',
  args: stories['API playground'],
  render: (args) => Template(args),
};

export const TestingSnapshot: StoryObj = {
  name: 'Testing - Snapshot',
  args: stories,
  parameters: {
    chromatic: { disableSnapshot: false },
  },
  render: (args) => TemplateStoriesFactory(Template)(args as any),
};
