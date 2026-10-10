import { Meta } from '@storybook/react-webpack5';
import VPCBanner from './index';

export default {
  title: 'components/VPCBanner',
  component: VPCBanner,
  parameters: {
    layout: 'centered',
  },
} as Meta<typeof VPCBanner>;

export const Showcase = () => (
  <VPCBanner onClose={() => window.alert('Close banner clicked')} />
);
