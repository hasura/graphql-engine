import { StoryObj, Meta } from '@storybook/react';
import { expect } from '@storybook/jest';
import { screen, within } from '@storybook/testing-library';
import { action } from '@storybook/addon-actions';
import { FaAddressBook, FaAirFreshener } from 'react-icons/fa';
import { hasuraToast } from '../Toasts';
import { BreadcrumbItem, Breadcrumbs } from './Breadcrumbs';

export default {
  title: 'components/Breadcrumbs 🧬',

  decorators: [
    Story => (
      <div className="p-4 flex gap-5 items-center max-w-screen">{Story()}</div>
    ),
  ],
} as Meta<typeof Breadcrumbs>;

const autoByeToast = (message: string) => {
  hasuraToast({ title: message, toastOptions: { duration: 1500 } });
  action(message);
};

const breadcrumbs: BreadcrumbItem[] = [
  {
    title: 'clickable w/ icon',
    icon: <FaAddressBook />,
    onClick: () => autoByeToast('foo'),
  },
  {
    title: 'clickable no icon',

    onClick: () => autoByeToast('no icon'),
  },
  {
    title: 'not clickable',
  },
  'string item',
  {
    title: 'last item',
    icon: <FaAirFreshener />,
  },
];

export const Basic: StoryObj<typeof Breadcrumbs> = {
  render: () => {
    return (
      <div>
        <Breadcrumbs items={breadcrumbs} />
        <div className="w-full h-full bg-slate-200 p-3">
          Some other content to visualize spacing
        </div>
      </div>
    );
  },

  name: '🧰 Basic Usage',

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await expect(await screen.findByTestId('breadcrumbs')).toBeInTheDocument();
    await expect(
      await canvas.getByText('clickable w/ icon')
    ).toBeInTheDocument();
    await expect(
      await canvas.getByText('clickable no icon')
    ).toBeInTheDocument();
    await expect(await canvas.getByText('not clickable')).toBeInTheDocument();
    await expect(await canvas.getByText('string item')).toBeInTheDocument();
    await expect(await canvas.getByText('last item')).toHaveClass(
      'text-yellow-500'
    );
  },
};
