import type { Meta, StoryObj } from '@storybook/react';

import { CollapsibleFieldWrapper } from './CollapsibleFieldWrapper';

export default {
  title: 'Features/OpenTelemetry/Form/CollapsibleFieldWrapper',
  component: CollapsibleFieldWrapper,
} as Meta<typeof CollapsibleFieldWrapper>;

export const Default: StoryObj<typeof CollapsibleFieldWrapper> = {
  name: '💠 Default',
  args: {
    inputFieldName: 'Input Field Name',
    label: 'Input Field Label',
    tooltip: 'Input Field Tooltip',
    learnMoreLink: 'https://hasura.io/docs',
    children: <i>Children go here</i>,
  },
};
