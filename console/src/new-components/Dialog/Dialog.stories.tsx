import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { Dialog, DialogProps, FooterProps } from './Dialog';

export default {
  title: 'components/Dialog',
  component: Dialog,
  argTypes: {
    onSubmit: { action: true },
    onClose: { action: true },
  },
} as ComponentMeta<typeof Dialog>;

export const Base: Story<DialogProps> = args => <Dialog {...args} />;

Base.args = {
  children: 'Body of the modal',
};

export const Complete: Story<DialogProps & FooterProps> = args => (
  <Dialog
    hasBackdrop={args.hasBackdrop}
    title={args.title}
    description={args.description}
    onClose={args.onClose}
  >
    <>
      <div>I am the body!</div>
      <Dialog.Footer
        callToDeny={args.callToDeny}
        callToAction={args.callToAction}
        onClose={args.onClose}
        onSubmit={args.onSubmit}
        isLoading={args.isLoading}
      />
    </>
  </Dialog>
);

Complete.args = {
  title: 'Title',
  description: 'Description',
  callToDeny: 'Cancel',
  callToAction: 'Submit',
  hasBackdrop: true,
  isLoading: false,
};
