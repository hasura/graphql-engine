import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { registerEETrialLicenseActiveMutation } from '../../mocks/registration.mock';

import { FormWrapper } from './FormWrapper';

export default {
  title: 'features / EETrial / Activate EE Form / Form Wrapper 🧬️',
  component: FormWrapper,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof FormWrapper>;

export const Default: ComponentStory<typeof FormWrapper> = () => (
  <FormWrapper onFormClose={() => {}} showBenefitsView />
);
Default.storyName = '💠 Default';
Default.parameters = {
  msw: [registerEETrialLicenseActiveMutation],
};

export const LicenseExpired: ComponentStory<typeof FormWrapper> = () => (
  <FormWrapper onFormClose={() => {}} showBenefitsView />
);
LicenseExpired.storyName = '💠 License Expired';
