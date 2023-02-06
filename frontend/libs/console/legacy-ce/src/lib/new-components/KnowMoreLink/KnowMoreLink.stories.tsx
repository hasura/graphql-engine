import type { ComponentPropsWithoutRef } from 'react';
import type { ComponentMeta, ComponentStory } from '@storybook/react';

import * as React from 'react';

import { KnowMoreLink } from './KnowMoreLink';

export default {
  title: 'components/KnowMoreLink ⚛️',
  component: KnowMoreLink,
  parameters: {
    docs: {
      description: {
        component: `Useful to link the docs of external resource containing more information`,
      },
      source: { type: 'code' },
    },
  },
} as ComponentMeta<typeof KnowMoreLink>;

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

export const Basic: ComponentStory<typeof KnowMoreLink> = args => (
  <KnowMoreLink {...args} />
);
Basic.storyName = '🧰 Basic';

const basicArgs: ComponentPropsWithoutRef<typeof KnowMoreLink> = {
  href: 'https://hasura.io/docs',
};
Basic.args = basicArgs;

// #endregion
