import React from 'react';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import {
  FiSearch,
  FiPhoneCall,
  FiAirplay,
  FiActivity,
  FiAnchor,
  FiPaperclip,
  FiClipboard,
} from 'react-icons/fi';
import { AiTwotoneDelete } from 'react-icons/ai';
import { IconType } from 'react-icons/lib';
import { Button } from '.';

export default {
  title: 'components/Buttons',
  component: Button,
} as ComponentMeta<typeof Button>;

const icons: Record<any, IconType> = {
  search: FiSearch,
  delete: AiTwotoneDelete,
  phone: FiPhoneCall,
  airplay: FiAirplay,
  activity: FiActivity,
  anchor: FiAnchor,
  clipboard: FiClipboard,
  paperclip: FiPaperclip,
};

export const showcase = () => (
  <main>
    <div>
      <h1 className="prose-2xl">Simple Buttons</h1>
      <div className="flex items-center mb-10 mt-2 content-evenly space-x-sm">
        <Button>Default</Button>
        <Button mode="default" size="sm">
          Default (small)
        </Button>
        <Button mode="destructive" size="md">
          Destructive
        </Button>
        <Button mode="destructive" size="sm">
          Destructive (small)
        </Button>
        <Button mode="primary">Primary</Button>
        <Button mode="primary" size="sm">
          Primary (small)
        </Button>
        <Button mode="primary" disabled>
          Disabled
        </Button>
        <Button mode="primary" disabled size="sm">
          Disabled (small)
        </Button>
        <Button mode="destructive" disabled>
          Disabled
        </Button>
        <Button mode="destructive" disabled size="sm">
          Disabled (small)
        </Button>
        <Button disabled>Disabled</Button>
        <Button disabled size="sm">
          Disabled (small)
        </Button>
      </div>
    </div>
    <div>
      <h1 className="prose-2xl">Buttons with icons</h1>
      <div className="flex items-center mb-10 mt-2 content-evenly space-x-sm">
        <Button icon={<FiSearch />} iconPosition="start">
          Default
        </Button>
        <Button
          icon={<FiPhoneCall />}
          iconPosition="end"
          mode="default"
          size="sm"
        >
          Default (small)
        </Button>
        <Button
          icon={<FiAirplay />}
          iconPosition="start"
          mode="destructive"
          size="md"
        >
          Destructive
        </Button>
        <Button
          icon={<FiActivity />}
          iconPosition="end"
          mode="destructive"
          size="sm"
        >
          Destructive (small)
        </Button>
        <Button icon={<FiAnchor />} iconPosition="start" mode="primary">
          Primary
        </Button>
        <Button
          icon={<FiAnchor />}
          iconPosition="start"
          mode="primary"
          size="sm"
        >
          Primary (small)
        </Button>
      </div>
    </div>

    <div>
      <h1 className="prose-2xl">Disabled Buttons with icons</h1>
      <div className="flex items-center mb-10 mt-2 content-evenly space-x-sm">
        <Button icon={<FiPaperclip />} iconPosition="start" disabled>
          Disabled
        </Button>
        <Button icon={<FiPaperclip />} iconPosition="start" disabled size="sm">
          Disabled (small)
        </Button>
        <Button
          icon={<FiPaperclip />}
          iconPosition="start"
          mode="destructive"
          disabled
        >
          Disabled
        </Button>
        <Button
          icon={<FiPaperclip />}
          iconPosition="start"
          mode="destructive"
          size="sm"
          disabled
        >
          Disabled (small)
        </Button>
        <Button
          icon={<FiPaperclip />}
          iconPosition="start"
          mode="primary"
          disabled
        >
          Disabled
        </Button>
        <Button
          icon={<FiPaperclip />}
          iconPosition="start"
          mode="primary"
          size="sm"
          disabled
        >
          Disabled (small)
        </Button>
      </div>
    </div>
    <div>
      <h1 className="prose-2xl">Buttons with loading state</h1>
      <div className="flex items-center mb-10 mt-2 content-evenly space-x-sm">
        <Button icon={<FiSearch />} iconPosition="start" isLoading>
          Default
        </Button>
        <Button
          icon={<FiSearch />}
          iconPosition="end"
          mode="default"
          size="md"
          isLoading
          loadingText="Searching"
        >
          Default
        </Button>
        <Button icon={<FiSearch />} iconPosition="start" isLoading size="sm">
          Default
        </Button>
        <Button
          icon={<FiSearch />}
          iconPosition="end"
          mode="default"
          size="sm"
          isLoading
          loadingText="Searching"
        >
          Default
        </Button>
        <Button
          icon={<FiSearch />}
          iconPosition="start"
          mode="destructive"
          size="md"
          isLoading
        >
          Destructive
        </Button>

        <Button
          icon={<AiTwotoneDelete />}
          iconPosition="end"
          mode="destructive"
          loadingText="Deleting table"
          size="md"
          isLoading
        >
          Destructive
        </Button>

        <Button
          icon={<FiSearch />}
          iconPosition="start"
          mode="destructive"
          size="sm"
          isLoading
        >
          Destructive
        </Button>

        <Button
          icon={<AiTwotoneDelete />}
          iconPosition="end"
          mode="destructive"
          loadingText="Deleting table"
          size="sm"
          isLoading
        >
          Destructive
        </Button>
        <Button
          icon={<FiSearch />}
          iconPosition="start"
          mode="primary"
          isLoading
        >
          Primary
        </Button>
        <Button
          icon={<FiSearch />}
          iconPosition="start"
          mode="primary"
          isLoading
          loadingText="Creating schema"
        >
          Primary
        </Button>
        <Button
          icon={<FiSearch />}
          iconPosition="start"
          mode="primary"
          size="sm"
          isLoading
        >
          Primary
        </Button>
        <Button
          icon={<FiSearch />}
          iconPosition="start"
          mode="primary"
          size="sm"
          isLoading
          loadingText="Creating schema"
        >
          Primary
        </Button>
      </div>
    </div>
  </main>
);

export const Playground: ComponentStory<typeof Button> = ({
  icon,
  ...args
}) => {
  const Icon = icons[icon as any];
  return <Button icon={<Icon />} {...args} />;
};

Playground.args = {
  children: 'Contact',
  disabled: false,
  iconPosition: 'start',
  isLoading: false,
  loadingText: 'Contacting',
};

Playground.parameters = {
  // Disable storybook for playground stories
  chromatic: { disableSnapshot: true },
};

Playground.argTypes = {
  icon: {
    defaultValue: 'phone',
    options: Object.keys(icons),
  },
};
