import React from 'react';
import { ComponentMeta } from '@storybook/react';

import {
  RemoteRelOption,
  RemoteRelRadioCardPicker,
} from './RemoteRelRadioCardPicker';

export default {
  title: 'components/RemoteRelRadioCard',
  component: RemoteRelRadioCardPicker,
} as ComponentMeta<typeof RemoteRelRadioCardPicker>;

export const Playground = () => {
  const [value, setValue] = React.useState('remoteSchema' as RemoteRelOption);
  return <RemoteRelRadioCardPicker value={value} onChange={setValue} />;
};
