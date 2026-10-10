import { action } from 'storybook/actions';
import { expect, screen, userEvent, within } from 'storybook/test';
import { StoryFn, StoryObj, Meta } from '@storybook/react-webpack5';
import { faker } from '@faker-js/faker';
import React from 'react';
import { GiHamburgerMenu } from 'react-icons/gi';
import { Badge } from '../../Badge';
import { Button } from '../../Button';
import { DropdownMenu } from '.';

export default {
  title: 'components/dropdown/Advanced Dropdown Menu 🧬',
  parameters: {
    chromatic: { disableSnapshot: true },
  },
  decorators: [
    (Story) => (
      <div className="p-4 flex gap-5 items-center max-w-screen">{Story()}</div>
    ),
  ],
  component: DropdownMenu.Root,
  args: {
    defaultOpen: false,
  },
} as Meta<typeof DropdownMenu.Root>;

// extracting trigger to component to not litter the story examples with boilerplate
const Trigger = ({ labelText = 'Click here' }: { labelText?: string }) => (
  <div className="relative">
    <Button leftIcon={GiHamburgerMenu} data-testid="trigger" />
    <span className="absolute whitespace-nowrap ml-2 top-1/2 left-full -translate-y-1/2">{`<------ ${labelText}`}</span>
  </div>
);

export const BasicItems: StoryObj<typeof DropdownMenu.Root> = {
  render: () => {
    return (
      <div className="w-full">
        <DropdownMenu.Root
          items={[
            <DropdownMenu.Item onClick={action(`New Tab...`)}>
              New Tab
            </DropdownMenu.Item>,
            <DropdownMenu.Item onClick={action(`New Window...`)}>
              New Window
            </DropdownMenu.Item>,
          ]}
        >
          <Trigger />
        </DropdownMenu.Root>
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

export const DangerousItem: StoryFn<typeof DropdownMenu.Root> = () => {
  return (
    <div className="w-full">
      <DropdownMenu.Root
        items={[
          <DropdownMenu.Item
            color="red"
            onClick={action(`This is scary! Why did you click it?!`)}
          >
            Dangerous!
          </DropdownMenu.Item>,
        ]}
      >
        <Trigger />
      </DropdownMenu.Root>
    </div>
  );
};

export const DisabledItem: StoryObj<typeof DropdownMenu.Root> = {
  render: () => {
    return (
      <div className="w-full">
        <DropdownMenu.Root
          items={[
            <DropdownMenu.Item
              onClick={action(`New Private Window...`)}
              disabled
            >
              New Private Window
            </DropdownMenu.Item>,
          ]}
        >
          <Trigger />
        </DropdownMenu.Root>
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

export const Separators: StoryFn<typeof DropdownMenu.Root> = () => {
  return (
    <div className="w-full">
      <DropdownMenu.Root
        items={[
          <DropdownMenu.Item onClick={action(`New Tab...`)}>
            New Tab
          </DropdownMenu.Item>,
          <DropdownMenu.Item onClick={action(`New Window...`)}>
            New Window
          </DropdownMenu.Item>,
          <DropdownMenu.Separator />,
          <DropdownMenu.Item onClick={action(`New Private Window...`)} disabled>
            New Private Window
          </DropdownMenu.Item>,
          <DropdownMenu.Separator />,
          <DropdownMenu.Item
            color="red"
            onClick={action(`This is scary! Why did you click it?!`)}
          >
            Dangerous!
          </DropdownMenu.Item>,
        ]}
      >
        <Trigger />
      </DropdownMenu.Root>
    </div>
  );
};

export const Labels: StoryFn<typeof DropdownMenu.Root> = () => {
  return (
    <div className="w-full">
      <DropdownMenu.Root
        items={[
          <DropdownMenu.Label>Basic Options</DropdownMenu.Label>,
          <DropdownMenu.Item onClick={action(`New Tab...`)}>
            New Tab
          </DropdownMenu.Item>,
          <DropdownMenu.Item onClick={action(`New Window...`)}>
            New Window
          </DropdownMenu.Item>,
          <DropdownMenu.Separator />,
          <DropdownMenu.Label>Advanced Options</DropdownMenu.Label>,
          <DropdownMenu.Item onClick={action(`New Private Window...`)} disabled>
            New Private Window
          </DropdownMenu.Item>,
          <DropdownMenu.Item onClick={action(`New Private Window...`)}>
            New Extra Private Window
          </DropdownMenu.Item>,
          <DropdownMenu.Separator />,
          <DropdownMenu.Label>Dangerous Options</DropdownMenu.Label>,
          <DropdownMenu.Item
            color="red"
            onClick={action(`This is scary! Why did you click it?!`)}
          >
            Explode Computer!
          </DropdownMenu.Item>,
          <DropdownMenu.Item
            color="red"
            onClick={action(`This is scary! Why did you click it?!`)}
          >
            Explode Space Station!
          </DropdownMenu.Item>,
        ]}
      >
        <Trigger />
      </DropdownMenu.Root>
    </div>
  );
};

export const CheckItem: StoryObj<typeof DropdownMenu.Root> = {
  render: () => {
    const [bookmarksChecked, setBookmarksChecked] = React.useState(true);
    const [urlsChecked, setUrlsChecked] = React.useState(false);

    const menuState = () => (
      <div className="w-full font-[monospace] ">
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
        <DropdownMenu.Root
          items={[
            <DropdownMenu.CheckboxItem
              checked={bookmarksChecked}
              onCheckedChange={setBookmarksChecked}
            >
              Show Bookmarks
            </DropdownMenu.CheckboxItem>,
            <DropdownMenu.CheckboxItem
              checked={urlsChecked}
              onCheckedChange={setUrlsChecked}
            >
              Show Full URLs
            </DropdownMenu.CheckboxItem>,
          ]}
        >
          <Trigger />
        </DropdownMenu.Root>
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

export const RadioItems: StoryObj<typeof DropdownMenu.Root> = {
  render: () => {
    const [person, setPerson] = React.useState('jon');

    const menuState = () => (
      <div className="w-full font-[monospace] ">
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
        <DropdownMenu.Root
          items={[
            <DropdownMenu.Label>People</DropdownMenu.Label>,
            <DropdownMenu.RadioGroup
              value={person}
              onValueChange={(p) => setPerson(p)}
            >
              <DropdownMenu.RadioItem value="luke">
                Luke Skywalker
              </DropdownMenu.RadioItem>
              <DropdownMenu.RadioItem value="darth">
                Darth Vader
              </DropdownMenu.RadioItem>
            </DropdownMenu.RadioGroup>,
          ]}
        >
          <Trigger />
        </DropdownMenu.Root>
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

export const DefaultOpen: StoryObj<typeof DropdownMenu.Root> = {
  render: (props) => {
    return (
      <div className="w-full">
        <DropdownMenu.Root {...props} items={[]}>
          <Trigger />
        </DropdownMenu.Root>
      </div>
    );
  },

  play: async () => {
    await expect(await screen.findByText('Use Wisely!')).toBeInTheDocument();
  },

  args: {
    options: {
      root: {
        defaultOpen: true,
      },
    },
  },
};

export const SubMenu: StoryObj<typeof DropdownMenu.Root> = {
  render: () => {
    return (
      <div className="w-full">
        <DropdownMenu.Root
          items={[
            <DropdownMenu.Item onClick={action(`New Tab...`)}>
              Top Level Item
            </DropdownMenu.Item>,
            <DropdownMenu.Sub
              items={[
                <DropdownMenu.Item onClick={action(`Save Page As...`)}>
                  Nested Item
                </DropdownMenu.Item>,
              ]}
            >
              A Sub Menu
            </DropdownMenu.Sub>,
            <DropdownMenu.Sub
              items={[
                <DropdownMenu.Item onClick={action(`Save Page As...`)}>
                  Super Nested Item
                </DropdownMenu.Item>,
              ]}
            >
              A Sub Sub Menu
            </DropdownMenu.Sub>,
          ]}
        >
          <Trigger />
        </DropdownMenu.Root>
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
      await screen.findByText('Super Nested Item'),
    ).toBeInTheDocument();
  },
};

export const LotsOfItems: StoryFn<typeof DropdownMenu.Root> = () => {
  const data = React.useRef(faker.helpers.uniqueArray(faker.word.noun, 100));
  return (
    <div className="w-full">
      <DropdownMenu.Root
        options={{
          root: {
            defaultOpen: true,
          },
        }}
        items={[
          <DropdownMenu.Sub
            items={data.current.map((w) => (
              <DropdownMenu.Item>{w}</DropdownMenu.Item>
            ))}
          >
            Sub Menu
          </DropdownMenu.Sub>,
        ]}
      >
        <Trigger />
      </DropdownMenu.Root>
    </div>
  );
};

export const CompleteExample: StoryObj<typeof DropdownMenu.Root> = {
  render: (args) => {
    const [bookmarksChecked, setBookmarksChecked] = React.useState(true);
    const [urlsChecked, setUrlsChecked] = React.useState(false);
    const [person, setPerson] = React.useState('darth');

    const menuState = () => (
      <>
        <div className="mb-2 text-lg">
          This is a complete example of how to implement the advanced drop down.
        </div>
        <div className="w-full font-[monospace] ">
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
        <DropdownMenu.Root
          {...args}
          items={[
            <DropdownMenu.Label>Basic Options</DropdownMenu.Label>,
            <DropdownMenu.Item shortcut="⌘+T" onClick={action(`New Tab...`)}>
              New Tab
            </DropdownMenu.Item>,
            <DropdownMenu.Item shortcut="⌘+N" onClick={action(`New Window...`)}>
              New Window
            </DropdownMenu.Item>,
            <DropdownMenu.Item
              shortcut="⇧+⌘+N"
              onClick={action(`New Private Window...`)}
              disabled
            >
              New Private Window
            </DropdownMenu.Item>,
            <DropdownMenu.Item
              color="red"
              onClick={action(`This is scary! Why did you click it?!`)}
            >
              Dangerous!
            </DropdownMenu.Item>,
            <DropdownMenu.Sub
              items={[
                <DropdownMenu.Item
                  shortcut="⌘+S"
                  onClick={action(`Save Page As...`)}
                >
                  Save Page As...
                </DropdownMenu.Item>,
                <DropdownMenu.Item
                  shortcut="⌘+D"
                  onClick={action(`Create Bookmark...`)}
                >
                  Create Bookmark
                </DropdownMenu.Item>,
                <DropdownMenu.Item onClick={action(`New Window...`)}>
                  New Window
                </DropdownMenu.Item>,
                <DropdownMenu.Separator />,
                <DropdownMenu.Item onClick={action(`Developer Tools...`)}>
                  Developer Tools
                </DropdownMenu.Item>,
              ]}
            >
              More Tools
            </DropdownMenu.Sub>,
            <DropdownMenu.Separator />,
            <DropdownMenu.Label>Save Stuff</DropdownMenu.Label>,
            <DropdownMenu.CheckboxItem
              shortcut="⌘+B"
              checked={bookmarksChecked}
              onCheckedChange={(checked) => setBookmarksChecked(checked)}
            >
              Show Bookmarks{' '}
            </DropdownMenu.CheckboxItem>,
            <DropdownMenu.CheckboxItem
              checked={urlsChecked}
              onCheckedChange={(checked) => setUrlsChecked(checked)}
            >
              Show Full URLs
            </DropdownMenu.CheckboxItem>,
            <DropdownMenu.Separator />,
            <DropdownMenu.Label>People</DropdownMenu.Label>,
            <DropdownMenu.RadioGroup
              value={person}
              onValueChange={(p) => setPerson(p)}
            >
              <DropdownMenu.RadioItem value="darth">
                Darth Vader
              </DropdownMenu.RadioItem>
              <DropdownMenu.RadioItem value="luke">
                Luke Skywalker
              </DropdownMenu.RadioItem>
            </DropdownMenu.RadioGroup>,
          ]}
        >
          <Trigger />
        </DropdownMenu.Root>
      </div>
    );
  },
};
