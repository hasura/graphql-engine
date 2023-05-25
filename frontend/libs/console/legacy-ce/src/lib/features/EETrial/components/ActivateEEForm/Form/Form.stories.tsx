import React from 'react';
import { StoryFn, StoryObj, Meta } from '@storybook/react';
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
} as Meta<typeof Form>;

export const Default: StoryObj<typeof Form> = {
  render: () => (
    <Dialog size="sm" onClose={() => {}} hasBackdrop>
      <Form onSuccess={() => {}} />
    </Dialog>
  ),

  name: '💠 Activate Existing License',

  parameters: {
    msw: [registerEETrialLicenseActiveMutation],
  },
};

export const GraphqlError: StoryObj<typeof Form> = {
  render: () => (
    <Dialog size="sm" onClose={() => {}} hasBackdrop>
      <>
        <div className="px-md mt-1 text-red-500 text-lg">
          Tip: Fill and submit the form to see error states.
        </div>
        <Form onSuccess={() => {}} />
      </>
    </Dialog>
  ),

  name: '💠 GraphqlError',

  parameters: {
    msw: [registerEETrialErrorMutation],
  },
};

export const LicenseAlreadyApplied: StoryObj<typeof Form> = {
  render: () => (
    <Dialog size="sm" onClose={() => {}} hasBackdrop>
      <>
        <div className="px-md mt-1 text-red-500 text-lg">
          Tip: Fill and submit the form to see error states.
        </div>
        <Form onSuccess={() => {}} />
      </>
    </Dialog>
  ),

  name: '💠 License Already Applied',

  parameters: {
    msw: [registerEETrialLicenseAlreadyAppliedMutation],
  },
};

export const ActivateExistingLicense: StoryFn<typeof Form> = () => (
  <Dialog size="sm" onClose={() => {}} hasBackdrop>
    <Form onSuccess={() => {}} formState="activate" />
  </Dialog>
);
