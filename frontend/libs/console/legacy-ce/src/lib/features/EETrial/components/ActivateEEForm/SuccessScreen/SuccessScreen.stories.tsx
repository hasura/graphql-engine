import React from 'react';
import { StoryObj, Meta } from '@storybook/react-webpack5';

import { SuccessScreen } from './SuccessScreen';
import { Dialog } from '@hasura/shared/ui';

export default {
  title: 'features / EETrial / Activate EE Form / Success Screen 🧬️',
  component: SuccessScreen,
} as Meta<typeof SuccessScreen>;

export const Demo: StoryObj<typeof SuccessScreen> = {
  render: () => {
    return (
      <Dialog size="sm" onClose={() => {}}>
        <SuccessScreen />
      </Dialog>
    );
  },

  name: '💠 Demo',
};
