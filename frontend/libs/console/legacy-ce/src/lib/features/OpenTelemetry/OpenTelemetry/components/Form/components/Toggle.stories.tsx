import type { ComponentPropsWithoutRef } from 'react';
import type { StoryObj, Meta } from '@storybook/react';

import * as React from 'react';

import { z } from 'zod';
import { expect } from '@storybook/jest';
import { SimpleForm } from '../../../../../../new-components/Form';
import { userEvent, within } from '@storybook/testing-library';

import { Toggle } from './Toggle';

export default {
  title: 'Features/OpenTelemetry/Toggle',
  component: Toggle,

  parameters: {
    docs: {
      description: {
        component: `A component that connects the standalone Switch with the Hasura Form APIs.`,
      },
      source: { type: 'code' },
    },
  },
} as Meta<typeof Toggle>;

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const basicStoryArgs: ComponentPropsWithoutRef<typeof Toggle> = {
  name: 'status',
  label: 'Status',
};

export const Basic: StoryObj<typeof Toggle> = {
  render: args => {
    const schema = z.object({ status: z.boolean() });

    return (
      <SimpleForm schema={schema} onSubmit={() => {}}>
        <Toggle {...args} />
      </SimpleForm>
    );
  },

  name: '🧰 Basic',
  args: basicStoryArgs,
};

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const withoutLabelStoryArgs: ComponentPropsWithoutRef<typeof Toggle> = {
  name: 'status',
};

export const WithoutLabel: StoryObj<typeof Toggle> = {
  render: args => {
    const schema = z.object({ status: z.boolean() });

    return (
      <SimpleForm schema={schema} onSubmit={() => {}}>
        <Toggle {...args} />
      </SimpleForm>
    );
  },

  name: '🎭 Variant - Without label',
  args: withoutLabelStoryArgs,
};

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const disabledStoryArgs: ComponentPropsWithoutRef<typeof Toggle> = {
  name: 'status',
  label: 'Status',
  disabled: true,
};

export const Disabled: StoryObj<typeof Toggle> = {
  render: args => {
    const schema = z.object({ status: z.boolean() });

    return (
      <SimpleForm schema={schema} onSubmit={() => {}}>
        <Toggle {...args} />
      </SimpleForm>
    );
  },

  name: '🎭 Variant - Disabled',
  args: disabledStoryArgs,
};

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const writtenStatusStoryArgs: ComponentPropsWithoutRef<typeof Toggle> = {
  name: 'status',
  label: 'Status',
  writtenStatus: { true: 'Enabled', false: 'Disabled' },
};

export const WrittenStatus: StoryObj<typeof Toggle> = {
  render: args => {
    const schema = z.object({ status: z.boolean() });

    return (
      <SimpleForm schema={schema} onSubmit={() => {}}>
        <Toggle {...args} />
      </SimpleForm>
    );
  },

  name: '🎭 Variant - With written status',
  args: writtenStatusStoryArgs,
};

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const testingScalabilityStoryArgs: ComponentPropsWithoutRef<typeof Toggle> = {
  name: 'status',
  label:
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.',
  writtenStatus: {
    false:
      'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.',
    true: 'Lorem ipsum',
  },
  description:
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.',
  tooltip:
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.',
};

export const TestingScalability: StoryObj<typeof Toggle> = {
  render: args => {
    const schema = z.object({ status: z.boolean() });

    return (
      <SimpleForm schema={schema} onSubmit={() => {}}>
        <Toggle {...args} />
        <span>
          <i>After-toggle text</i>
        </span>
      </SimpleForm>
    );
  },

  name: '🧪 Testing - Scalability',
  args: testingScalabilityStoryArgs,
};

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const happyPathStoryArgs: ComponentPropsWithoutRef<typeof Toggle> = {
  name: 'status',
  label: 'Status',
  writtenStatus: { true: 'Enabled', false: 'Disabled' },
};

export const HappyPath: StoryObj<typeof Toggle> = {
  render: args => {
    const schema = z.object({ status: z.boolean() });

    return (
      <SimpleForm schema={schema} onSubmit={() => {}}>
        <Toggle {...args} />
      </SimpleForm>
    );
  },

  name: '🧪 Testing - When clicked, should enable',
  args: happyPathStoryArgs,

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const label = await canvas.getByText('Status');

    // STEP: check the initial disabled status
    const writtenStatus = await canvas.getByText('Disabled');

    await expect(writtenStatus).toBeVisible();

    // STEP: CLick the toggle
    await userEvent.click(label);

    // STEP: check the enabled status
    await expect(writtenStatus).toHaveTextContent('Enabled');
  },
};
