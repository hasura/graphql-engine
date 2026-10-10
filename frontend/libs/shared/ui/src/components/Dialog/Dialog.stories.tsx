import { StoryObj, Meta } from '@storybook/react-webpack5';
import { RiPlayFill } from 'react-icons/ri';
import { Dialog, DialogProps } from './Dialog';
import { hasuraToast } from '../Toasts';
import DialogFooter, { FooterProps } from './DialogFooter';

export default {
  title: 'components/Dialog',
  component: Dialog,
  argTypes: {
    onSubmit: { action: true },
    onClose: { action: true },
  },
} as Meta<typeof Dialog>;

export const Base: StoryObj<DialogProps> = {
  args: {
    children: 'Body of the modal',
  },
};

export const Complete: StoryObj<DialogProps & FooterProps> = {
  render: (args) => (
    <Dialog
      title={args.title}
      description={args.description}
      onClose={args.onClose}
    >
      <>
        <div>I am the body!</div>
        <DialogFooter
          callToDeny={args.callToDeny}
          callToAction={args.callToAction}
          callToDenyProps={{
            leftIcon: RiPlayFill,
          }}
          onClose={args.onClose}
          onSubmit={args.onSubmit}
          isLoading={args.isLoading}
          leftContent={args.leftContent}
        />
      </>
    </Dialog>
  ),

  args: {
    title: 'Title',
    description: 'Description',
    callToDeny: 'Cancel',
    callToAction: 'Submit',
    isLoading: false,
    leftContent: 'Learn more',
  },
};

export const CustomFooter: StoryObj<DialogProps & FooterProps> = {
  render: (args) => (
    <Dialog
      title={args.title}
      description={args.description}
      onClose={args.onClose}
      footer={args.footer}
    >
      <>
        <div>I am the body!</div>
      </>
    </Dialog>
  ),

  args: {
    title: 'Title',
    description: 'Description',
    callToDeny: 'Cancel',
    callToAction: 'Submit',
    isLoading: false,
    footer: <div>custom footer</div>,
  },
};

export const CollisionWithToast: StoryObj<DialogProps & FooterProps> = {
  render: (args) => (
    <Dialog
      title={args.title}
      description={args.description}
      onClose={args.onClose}
    >
      <>
        <div>I am the body!</div>
        <DialogFooter
          callToDeny={args.callToDeny}
          callToAction={args.callToAction}
          callToActionProps={{
            leftIcon: RiPlayFill,
          }}
          onClose={args.onClose}
          onSubmit={() =>
            hasuraToast({
              type: 'error',
              title: 'The toast title',
              message: 'The toast message',
            })
          }
          isLoading={args.isLoading}
        />
      </>
    </Dialog>
  ),

  args: {
    title: 'Title',
    description: 'Description',
    callToDeny: 'Cancel',
    callToAction: 'Show toast',
    isLoading: false,
  },
};
