import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { DockerConfigDialog } from './DockerConfigDialog';

export default {
  component: DockerConfigDialog,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof DockerConfigDialog>;

export const Primary: StoryObj<typeof DockerConfigDialog> = {
  render: args => {
    return (
      <div className="max-w-3xl">
        <DockerConfigDialog {...args} />
      </div>
    );
  },

  args: {
    onCancel: () => {},
    onSetupSuccess: () => {},
  },
};
