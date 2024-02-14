import { action } from '@storybook/addon-actions';
import { expect } from '@storybook/jest';
import { StoryFn, StoryObj, Meta } from '@storybook/react';
import { screen, userEvent, within } from '@storybook/testing-library';
import randomWords from 'random-words';
import React from 'react';
import { GiHamburgerMenu } from 'react-icons/gi';
import { DropDown } from '.';
import { Badge } from '../Badge';
import { Button } from '../Button';

export default {
  title: 'components/Advanced Dropdown Menu 🧬',
  parameters: {
    chromatic: { disableSnapshot: true },
  },
  decorators: [
    Story => (
      <div className="p-4 flex gap-5 items-center max-w-screen">{Story()}</div>
    ),
  ],
  component: DropDown.Root,
  args: {
    defaultOpen: false,
  },
} as Meta<typeof DropDown.Root>;

// extracting trigger to component to not litter the story examples with boilerplate
const Trigger = ({ labelText = 'Click here' }: { labelText?: string }) => (
  <div className="relative">
    <Button icon={<GiHamburgerMenu />} data-testid="trigger" />
    <span className="absolute whitespace-nowrap ml-2 top-1/2 left-full -translate-y-1/2">{`<------ ${labelText}`}</span>
  </div>
);

export const BasicItems: StoryObj<typeof DropDown.Root> = {
  render: () => {
    return (
      <div className="w-full">
        <DropDown.Root trigger={<Trigger />}>
          <DropDown.BasicItem onClick={action(`New Tab...`)}>
            New Tab
          </DropDown.BasicItem>
          <DropDown.BasicItem onClick={action(`New Window...`)}>
            New Window
          </DropDown.BasicItem>
        </DropDown.Root>
      </div>
    );
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(canvas.getByTestId('trigger'));

    await userEvent.click(await screen.findByText('New Tab'));

    await expect(screen.queryByText('New Tab')).not.toBeInTheDocument();
  },
};

export const DangerousItem: StoryFn<typeof DropDown.Root> = () => {
  return (
    <div className="w-full">
      <DropDown.Root trigger={<Trigger />}>
        <DropDown.BasicItem
          dangerous
          onClick={action(`This is scary! Why did you click it?!`)}
        >
          Dangerous!
        </DropDown.BasicItem>
      </DropDown.Root>
    </div>
  );
};

export const DisabledItem: StoryObj<typeof DropDown.Root> = {
  render: () => {
    return (
      <div className="w-full">
        <DropDown.Root trigger={<Trigger />}>
          <DropDown.BasicItem
            onClick={action(`New Private Window...`)}
            disabled
          >
            New Private Window
          </DropDown.BasicItem>
        </DropDown.Root>
      </div>
    );
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    await userEvent.click(canvas.getByTestId('trigger'));

    const el = await screen.findByText('New Private Window');

    await expect(el).toHaveStyle('pointer-events: none');

    await expect(el.parentElement).toHaveAttribute('aria-disabled', 'true');
  },
};

export const Separators: StoryFn<typeof DropDown.Root> = () => {
  return (
    <div className="w-full">
      <DropDown.Root trigger={<Trigger />}>
        <DropDown.BasicItem onClick={action(`New Tab...`)}>
          New Tab
        </DropDown.BasicItem>
        <DropDown.BasicItem onClick={action(`New Window...`)}>
          New Window
        </DropDown.BasicItem>
        <DropDown.Separator />
        <DropDown.BasicItem onClick={action(`New Private Window...`)} disabled>
          New Private Window
        </DropDown.BasicItem>
        <DropDown.Separator />
        <DropDown.BasicItem
          dangerous
          onClick={action(`This is scary! Why did you click it?!`)}
        >
          Dangerous!
        </DropDown.BasicItem>
      </DropDown.Root>
    </div>
  );
};

export const Labels: StoryFn<typeof DropDown.Root> = () => {
  return (
    <div className="w-full">
      <DropDown.Root trigger={<Trigger />}>
        <DropDown.Label>Basic Options</DropDown.Label>
        <DropDown.BasicItem onClick={action(`New Tab...`)}>
          New Tab
        </DropDown.BasicItem>
        <DropDown.BasicItem onClick={action(`New Window...`)}>
          New Window
        </DropDown.BasicItem>
        <DropDown.Separator />
        <DropDown.Label>Advanced Options</DropDown.Label>
        <DropDown.BasicItem onClick={action(`New Private Window...`)} disabled>
          New Private Window
        </DropDown.BasicItem>
        <DropDown.BasicItem onClick={action(`New Private Window...`)}>
          New Extra Private Window
        </DropDown.BasicItem>
        <DropDown.Separator />
        <DropDown.Label>Dangerous Options</DropDown.Label>
        <DropDown.BasicItem
          dangerous
          onClick={action(`This is scary! Why did you click it?!`)}
        >
          Explode Computer!
        </DropDown.BasicItem>
        <DropDown.BasicItem
          dangerous
          onClick={action(`This is scary! Why did you click it?!`)}
        >
          Explode Space Station!
        </DropDown.BasicItem>
      </DropDown.Root>
    </div>
  );
};

export const CheckItem: StoryObj<typeof DropDown.Root> = {
  render: () => {
    const [bookmarksChecked, setBookmarksChecked] = React.useState(true);
    const [urlsChecked, setUrlsChecked] = React.useState(false);

    const menuState = () => (
      <div className="w-full [font-family:monospace] ">
        <p className="mb-2 font-bold">Check States:</p>
        <div className="flex w-64 mb-2 justify-between items-center">
          <div>Bookmarks:</div>
          <Badge
            color={bookmarksChecked ? 'green' : 'gray'}
            data-testid="bookmark-state"
          >
            {bookmarksChecked.toString()}
          </Badge>
        </div>
        <div className="flex w-64 mb-2 justify-between items-center">
          <div>Full Urls: </div>
          <Badge color={urlsChecked ? 'green' : 'gray'} data-testid="url-state">
            {urlsChecked.toString()}
          </Badge>
        </div>
      </div>
    );

    return (
      <div className="w-full">
        {menuState()}
        <DropDown.Root trigger={<Trigger />}>
          <DropDown.CheckItem
            checked={bookmarksChecked}
            onCheckChange={setBookmarksChecked}
          >
            Show Bookmarks
          </DropDown.CheckItem>
          <DropDown.CheckItem
            checked={urlsChecked}
            onCheckChange={setUrlsChecked}
          >
            Show Full URLs
          </DropDown.CheckItem>
        </DropDown.Root>
      </div>
    );
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    const trigger = () => canvas.getByTestId('trigger');
    const bookmarkStatusElement = () => canvas.getByTestId('bookmark-state');
    const urlStatusElement = () => canvas.getByTestId('url-state');
    const showBookmarks = () => screen.findByText('Show Bookmarks');
    const showFullUrls = () => screen.findByText('Show Full URLs');

    // test starts here:
    await userEvent.click(trigger());

    await userEvent.click(await showBookmarks());

    // expect false b/c starts out true
    await expect(bookmarkStatusElement()).toHaveTextContent('false');

    await userEvent.click(await showFullUrls());

    // expect true b/c starts out false
    await expect(urlStatusElement()).toHaveTextContent('true');
  },
};

export const RadioItems: StoryObj<typeof DropDown.Root> = {
  render: () => {
    const [person, setPerson] = React.useState('jon');

    const menuState = () => (
      <div className="w-full [font-family:monospace] ">
        <p className="mb-2 font-bold">Radio State:</p>
        <div className="flex w-64 mb-2 justify-between items-center">
          <div className="capitalize">Selected Person:</div>
          <Badge color="blue" data-testid="selected-person">
            {person}
          </Badge>
        </div>
      </div>
    );

    return (
      <div className="w-full">
        {menuState()}
        <DropDown.Root trigger={<Trigger />}>
          <DropDown.RadioGroup
            label="People"
            value={person}
            onValueChange={p => setPerson(p)}
          >
            <DropDown.RadioItem value="luke">Luke Skywalker</DropDown.RadioItem>
            <DropDown.RadioItem value="darth">Darth Vader</DropDown.RadioItem>
          </DropDown.RadioGroup>
        </DropDown.Root>
      </div>
    );
  },

  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);

    const trigger = () => canvas.getByTestId('trigger');
    const personStatus = () => canvas.getByTestId('selected-person');
    const lukeRadio = () => screen.findByText('Luke Skywalker');
    const darthRadio = () => screen.findByText('Darth Vader');

    await userEvent.click(trigger());

    await userEvent.click(await lukeRadio());

    await expect(personStatus()).toHaveTextContent('luke');

    await userEvent.click(trigger());

    await userEvent.click(await darthRadio());

    await expect(personStatus()).toHaveTextContent('darth');
  },
};

export const DefaultOpen: StoryObj<typeof DropDown.Root> = {
  render: ({ defaultOpen }) => {
    return (
      <div className="w-full">
        <DropDown.Root defaultOpen={defaultOpen} trigger={<Trigger />}>
          <DropDown.BasicItem onClick={action(`New Tab...`)}>
            I am
          </DropDown.BasicItem>
          <DropDown.BasicItem onClick={action(`New Window...`)}>
            open
          </DropDown.BasicItem>
          <DropDown.BasicItem onClick={action(`New Private Window...`)}>
            by Default...
          </DropDown.BasicItem>
          <DropDown.BasicItem
            data-testid="dangerous-item"
            dangerous
            onClick={action(`This is scary! Why did you click it?!`)}
          >
            Use Wisely!
          </DropDown.BasicItem>
        </DropDown.Root>
      </div>
    );
  },

  play: async () => {
    await expect(await screen.findByText('Use Wisely!')).toBeInTheDocument();
  },

  args: {
    defaultOpen: true,
  },
};

export const RightSlot: StoryFn<typeof DropDown.Root> = () => {
  return (
    <div className="w-full">
      <div className="mb-4 text-lg">
        Put some content in the right margin next to a drop down item!
      </div>
      <DropDown.Root trigger={<Trigger />}>
        <DropDown.BasicItem onClick={action(`New Tab...`)}>
          New Tab
          <DropDown.ItemRightSlot>⌘+T</DropDown.ItemRightSlot>
        </DropDown.BasicItem>
        <DropDown.BasicItem onClick={action(`New Window...`)}>
          New Window
          <DropDown.ItemRightSlot>⌘+N</DropDown.ItemRightSlot>
        </DropDown.BasicItem>
        <DropDown.Separator />
        <DropDown.BasicItem onClick={action(`New Private Window...`)} disabled>
          New Private Window
          <DropDown.ItemRightSlot>⇧+⌘+N</DropDown.ItemRightSlot>
        </DropDown.BasicItem>
      </DropDown.Root>
    </div>
  );
};

export const SubMenu: StoryObj<typeof DropDown.Root> = {
  render: () => {
    return (
      <div className="w-full">
        <DropDown.Root trigger={<Trigger />}>
          <DropDown.BasicItem onClick={action(`New Tab...`)}>
            Top Level Item
          </DropDown.BasicItem>
          {/* To create a sub menu, just put any kind of item within the <DropDown.SubMenu /> component */}
          <DropDown.SubMenu label="A Sub Menu">
            <DropDown.BasicItem onClick={action(`Save Page As...`)}>
              Nested Item
            </DropDown.BasicItem>
            <DropDown.SubMenu label="A Sub Sub Menu">
              <DropDown.BasicItem onClick={action(`Save Page As...`)}>
                Super Nested Item
              </DropDown.BasicItem>
            </DropDown.SubMenu>
          </DropDown.SubMenu>
        </DropDown.Root>
      </div>
    );
  },

  play: async ({ canvasElement }) => {
    const c = within(canvasElement);

    await userEvent.click(c.getByTestId('trigger'));

    await userEvent.click(await screen.findByText('A Sub Menu'));

    await expect(await screen.findByText('Nested Item')).toBeInTheDocument();

    await userEvent.click(await screen.findByText('A Sub Sub Menu'));

    await expect(
      await screen.findByText('Super Nested Item')
    ).toBeInTheDocument();
  },
};

export const LotsOfItems: StoryFn<typeof DropDown.Root> = () => {
  const data = React.useRef(randomWords(100));
  return (
    <div className="w-full">
      <DropDown.Root defaultOpen trigger={<Trigger />} maxHeight={'75vh'}>
        <DropDown.SubMenu label="Sub Menu">
          {data.current.map(w => (
            <DropDown.BasicItem>{w}</DropDown.BasicItem>
          ))}
        </DropDown.SubMenu>
      </DropDown.Root>
    </div>
  );
};

export const CompleteExample: StoryObj<typeof DropDown.Root> = {
  render: args => {
    const [bookmarksChecked, setBookmarksChecked] = React.useState(true);
    const [urlsChecked, setUrlsChecked] = React.useState(false);
    const [person, setPerson] = React.useState('darth');

    const menuState = () => (
      <>
        <div className="mb-2 text-lg">
          This is a complete example of how to implement the advanced drop down.
        </div>
        <div className="w-full [font-family:monospace] ">
          <p className="mb-2 font-bold">Radio/Check States:</p>
          <div className="flex w-64 mb-2 justify-between items-center">
            <div>Show Bookmarks:</div>
            <Badge color={bookmarksChecked ? 'green' : 'gray'}>
              {bookmarksChecked.toString()}
            </Badge>
          </div>
          <div className="flex w-64 mb-2 justify-between items-center">
            <div>Show Full Urls: </div>
            <Badge color={urlsChecked ? 'green' : 'gray'}>
              {urlsChecked.toString()}
            </Badge>
          </div>
          <div className="flex w-64 mb-2 justify-between items-center">
            <div className="capitalize">Selected Person:</div>
            <Badge color="blue">{person}</Badge>
          </div>
        </div>
      </>
    );

    return (
      <div className="w-full">
        {menuState()}
        <DropDown.Root defaultOpen={args?.defaultOpen} trigger={<Trigger />}>
          <DropDown.Label>Basic Options</DropDown.Label>
          <DropDown.BasicItem onClick={action(`New Tab...`)}>
            New Tab
            <DropDown.ItemRightSlot>⌘+T</DropDown.ItemRightSlot>
          </DropDown.BasicItem>
          <DropDown.BasicItem onClick={action(`New Window...`)}>
            New Window
            <DropDown.ItemRightSlot>⌘+N</DropDown.ItemRightSlot>
          </DropDown.BasicItem>
          <DropDown.BasicItem
            onClick={action(`New Private Window...`)}
            disabled
          >
            New Private Window
            <DropDown.ItemRightSlot>⇧+⌘+N</DropDown.ItemRightSlot>
          </DropDown.BasicItem>
          <DropDown.BasicItem
            dangerous
            onClick={action(`This is scary! Why did you click it?!`)}
          >
            Dangerous!
          </DropDown.BasicItem>
          {/* To create a sub menu, just put any kind of item within the <DropDown.SubMenu /> component */}
          <DropDown.SubMenu label="More Tools">
            <DropDown.BasicItem onClick={action(`Save Page As...`)}>
              Save Page As...
              <DropDown.ItemRightSlot>⌘+S</DropDown.ItemRightSlot>
            </DropDown.BasicItem>
            <DropDown.BasicItem onClick={action(`Create Bookmark...`)}>
              Create Bookmark
              <DropDown.ItemRightSlot>⌘+D</DropDown.ItemRightSlot>
            </DropDown.BasicItem>
            <DropDown.BasicItem onClick={action(`New Window...`)}>
              New Window
            </DropDown.BasicItem>
            <DropDown.Separator />
            <DropDown.BasicItem onClick={action(`Developer Tools...`)}>
              Developer Tools
            </DropDown.BasicItem>
          </DropDown.SubMenu>
          <DropDown.Separator />
          <DropDown.Label>Save Stuff</DropDown.Label>
          <DropDown.CheckItem
            checked={bookmarksChecked}
            onCheckChange={checked => setBookmarksChecked(checked)}
          >
            Show Bookmarks <DropDown.ItemRightSlot>⌘+B</DropDown.ItemRightSlot>
          </DropDown.CheckItem>
          <DropDown.CheckItem
            checked={urlsChecked}
            onCheckChange={checked => setUrlsChecked(checked)}
          >
            Show Full URLs
          </DropDown.CheckItem>
          <DropDown.Separator />
          <DropDown.RadioGroup
            label="People"
            value={person}
            onValueChange={p => setPerson(p)}
          >
            <DropDown.RadioItem value="darth">Darth Vader</DropDown.RadioItem>
            <DropDown.RadioItem value="luke">Luke Skywalker</DropDown.RadioItem>
          </DropDown.RadioGroup>
        </DropDown.Root>
      </div>
    );
  },
};
