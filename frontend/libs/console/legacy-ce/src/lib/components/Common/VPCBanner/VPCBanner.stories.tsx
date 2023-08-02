import React from 'react';
import { Meta } from '@storybook/react';
import VPCBanner from './VPCBanner';

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
