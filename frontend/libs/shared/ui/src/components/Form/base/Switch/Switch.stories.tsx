import React from 'react';
import { Meta } from '@storybook/react-webpack5';
import { Switch } from './index';

export default {
  title: 'components/Switch',
  component: Switch,
} as Meta<typeof Switch>;

export const Off = () => <Switch value={false} />;

export const On = () => <Switch value={true} />;

export const Playground = () => {
  const [checked, setChecked] = React.useState(false);
  return <Switch value={checked} onChange={setChecked} />;
};
