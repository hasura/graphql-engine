import type { ComponentPropsWithoutRef } from 'react';
import type { ComponentMeta, ComponentStory } from '@storybook/react';

import * as React from 'react';

import { LearnMoreLink } from './LearnMoreLink';

export default {
  title: 'components/LearnMoreLink ⚛️',
  component: LearnMoreLink,
  parameters: {
    docs: {
      description: {
        component: `Useful to link the docs of external resource containing more information`,
      },
      source: { type: 'code' },
    },
  },
} as ComponentMeta<typeof LearnMoreLink>;

// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// DEFAULT STORY
// #region
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------

export const Basic: ComponentStory<typeof LearnMoreLink> = args => (
  <LearnMoreLink {...args} />
);
Basic.storyName = '🧰 Basic';

const basicArgs: ComponentPropsWithoutRef<typeof LearnMoreLink> = {
  href: 'https://hasura.io/docs',
};
Basic.args = basicArgs;

// #endregion

// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// CUSTOM TEXT
// #region
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------

// --------------------------------------------------
// STORY DEFINITION
// --------------------------------------------------

export const CustomText: ComponentStory<typeof LearnMoreLink> = args => (
  <LearnMoreLink {...args} />
);
CustomText.storyName = '🎭 Custom text';

const customTextArgs: ComponentPropsWithoutRef<typeof LearnMoreLink> = {
  href: 'https://hasura.io/docs',
  text: '(Read the docs)',
};
CustomText.args = customTextArgs;

// #endregion
