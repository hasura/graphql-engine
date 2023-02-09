import type { ComponentPropsWithoutRef } from 'react';
import type { ComponentMeta, ComponentStory } from '@storybook/react';

import * as React from 'react';

import { CollapsibleFieldWrapper } from './CollapsibleFieldWrapper';

export default {
  title: 'Features/OpenTelemetry/Form/CollapsibleFieldWrapper',
  component: CollapsibleFieldWrapper,
} as ComponentMeta<typeof CollapsibleFieldWrapper>;

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------
export const Default: ComponentStory<typeof CollapsibleFieldWrapper> = args => (
  <CollapsibleFieldWrapper {...args} />
);

Default.storyName = '💠 Default';

// --------------------------------------------------
// PROPS
// --------------------------------------------------
// Explicitly defining the story' args allows leveraging TS protection over them since story.args is
// a Partial<Props> and then developers cannot know that they break the story by changing the
// component props
const storyArgs: ComponentPropsWithoutRef<typeof CollapsibleFieldWrapper> = {
  inputFieldName: 'Input Field Name',
  label: 'Input Field Label',
  tooltip: 'Input Field Tooltip',
  learnMoreLink: 'https://hasura.io/docs',
  children: <i>Children go here</i>,
};
Default.args = storyArgs;

// --------------------------------------------------
// NOT TESTED
// --------------------------------------------------
// At the moment of writing, CollapsibleFieldWrapper does nothing but composing other components.
// That's why at the moment it's not tested at all
