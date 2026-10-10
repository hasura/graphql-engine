import React from 'react';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { ReactQueryDecorator } from '@hasura/shared/testing';
import { registerEETrialLicenseActiveMutation } from '../../mocks/registration.mock';
import { EnableEEButtonWrapper } from './EnableEEButton';
import { Button } from '@hasura/shared/ui';

export default {
  title: 'features / EETrial / EnableEEButtonWrapper 🧬️',
  component: EnableEEButtonWrapper,
  parameters: {
    msw: [registerEETrialLicenseActiveMutation],
  },
  decorators: [ReactQueryDecorator()],
} as Meta<typeof EnableEEButtonWrapper>;

export const Demo: StoryObj<typeof EnableEEButtonWrapper> = {
  render: () => (
    <EnableEEButtonWrapper showBenefitsView>
      <Button type="submit" mode="primary">
        Enable Enterprise
      </Button>
    </EnableEEButtonWrapper>
  ),

  name: '💠 Demo',
};
