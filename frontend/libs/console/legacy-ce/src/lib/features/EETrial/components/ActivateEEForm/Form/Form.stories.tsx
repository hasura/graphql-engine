import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { Dialog } from '../../../../../new-components/Dialog';
import {
  registerEETrialErrorMutation,
  registerEETrialLicenseActiveMutation,
  registerEETrialLicenseAlreadyAppliedMutation,
} from '../../../mocks/registration.mock';
import { Form } from './Form';

export default {
  title: 'features / EETrial / Activate EE Form / Form 🧬️',
  component: Form,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof Form>;

export const Default: ComponentStory<typeof Form> = () => (
  <Dialog size="sm" onClose={() => {}} hasBackdrop>
    <Form onSuccess={() => {}} />
  </Dialog>
);
Default.storyName = '💠 Default';
Default.parameters = {
  msw: [registerEETrialLicenseActiveMutation],
};

export const GraphqlError: ComponentStory<typeof Form> = () => (
  <Dialog size="sm" onClose={() => {}} hasBackdrop>
    <>
      <div className="px-md mt-1 text-red-500 text-lg">
        Tip: Fill and submit the form to see error states.
      </div>
      <Form onSuccess={() => {}} />
    </>
  </Dialog>
);
GraphqlError.storyName = '💠 GraphqlError';
GraphqlError.parameters = {
  msw: [registerEETrialErrorMutation],
};

export const LicenseAlreadyApplied: ComponentStory<typeof Form> = () => (
  <Dialog size="sm" onClose={() => {}} hasBackdrop>
    <>
      <div className="px-md mt-1 text-red-500 text-lg">
        Tip: Fill and submit the form to see error states.
      </div>
      <Form onSuccess={() => {}} />
    </>
  </Dialog>
);
LicenseAlreadyApplied.storyName = '💠 License Already Applied';
LicenseAlreadyApplied.parameters = {
  msw: [registerEETrialLicenseAlreadyAppliedMutation],
};

export const ActivateExistingLicense: ComponentStory<typeof Form> = () => (
  <Dialog size="sm" onClose={() => {}} hasBackdrop>
    <Form onSuccess={() => {}} formState="activate" />
  </Dialog>
);
Default.storyName = '💠 Activate Existing License';
Default.parameters = {
  msw: [registerEETrialLicenseActiveMutation],
};
