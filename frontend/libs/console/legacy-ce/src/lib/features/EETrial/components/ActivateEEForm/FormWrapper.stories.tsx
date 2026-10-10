import React from 'react';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { ReactQueryDecorator } from '@hasura/shared/testing';
import { registerEETrialLicenseActiveMutation } from '../../mocks/registration.mock';

import { FormWrapper } from './FormWrapper';

export default {
  title: 'features / EETrial / Activate EE Form / Form Wrapper 🧬️',
  component: FormWrapper,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof FormWrapper>;

export const Default: StoryObj<typeof FormWrapper> = {
  render: () => <FormWrapper onFormClose={() => {}} showBenefitsView />,

  name: '💠 Default',

  parameters: {
    msw: [registerEETrialLicenseActiveMutation],
  },
};

export const LicenseExpired: StoryObj<typeof FormWrapper> = {
  render: () => <FormWrapper onFormClose={() => {}} showBenefitsView />,

  name: '💠 License Expired',
};
