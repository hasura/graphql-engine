import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { registerEETrialLicenseActiveMutation } from '../../mocks/registration.mock';
import { EnableEEButtonWrapper } from './EnableEEButton';
import { Button } from '../../../../new-components/Button';

export default {
  title: 'features / EETrial / EnableEEButtonWrapper 🧬️',
  component: EnableEEButtonWrapper,
  parameters: {
    msw: [registerEETrialLicenseActiveMutation],
  },
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof EnableEEButtonWrapper>;

export const Demo: ComponentStory<typeof EnableEEButtonWrapper> = () => (
  <EnableEEButtonWrapper showBenefitsView>
    <Button type="submit" mode="primary">
      Enable Enterprise
    </Button>
  </EnableEEButtonWrapper>
);
Demo.storyName = '💠 Demo';
