import React from 'react';

import {
  Button,
  AlertBox,
  ToolTip,
  Heading,
  TextLink,
  Text,
  RadioButton,
  Checkbox,
  SwitchButton,
  Tabs,
  Spinner,
  Icon,
} from '../atoms';
import { Flex } from './styles';

// Dummy data for Tabs ********************** //

const dummytabsData = [
  {
    title: 'Title 1',
    tabContent: 'Content 1',
  },
  {
    title: 'Title 2',
    tabContent: 'Content 2',
  },
  {
    title: 'Title 3',
    tabContent: 'Content 3',
  },
  {
    title: 'Title 4',
    tabContent: 'Content 4',
  },
  {
    title: 'Title 5',
    tabContent: 'Content 5',
  },
];

export const UIComponents = () => (
  <React.Fragment>
    {/* Buttons ~ large size */}

    <Heading as="h2" mt="xl" mb="lg">
      Buttons
    </Heading>

    <Heading as="h3">{'<Button />'}</Heading>
    <Button m="lg">Default Button</Button>

    <Heading as="h3">{'<Button type="primary" />'}</Heading>
    <Button type="primary" m="lg">
      Primary Button
    </Button>

    <Heading as="h3">{'<Button type="primary" isLoading="true" />'}</Heading>
    <Button type="primary" m="lg" isLoading>
      Primary Button
    </Button>

    <Heading as="h3">{'<Button type="primary" size="large" />'}</Heading>
    <Button type="primary" size="large" m="lg">
      Primary Button
    </Button>

    <Heading as="h3">
      {'<Button type="primary" size="large" isLoading="true" />'}
    </Heading>
    <Button type="primary" size="large" m="lg" isLoading="true">
      Primary Button
    </Button>

    <Heading as="h3">{'<Button type="secondary" size="large" />'}</Heading>
    <Button type="secondary" size="large" m="lg">
      Secondary Button
    </Button>

    <Heading as="h3">
      {'<Button type="secondary" size="large" isLoading="true" />'}
    </Heading>
    <Button type="secondary" size="large" m="lg" isLoading="true">
      Secondary Button
    </Button>

    <Heading as="h3">{'<Button type="success" size="large" />'}</Heading>
    <Button type="success" size="large" m="lg">
      Success Button
    </Button>

    <Heading as="h3">{'<Button type="danger" size="large" />'}</Heading>
    <Button type="danger" size="large" m="lg">
      Danger Button
    </Button>

    <Heading as="h3">{'<Button type="warning" size="large" />'}</Heading>
    <Button type="warning" size="large" m="lg">
      Warning Button
    </Button>

    <Heading as="h3">{'<Button type="info" size="large" />'}</Heading>
    <Button type="info" size="large" m="lg">
      Info Button
    </Button>

    {/* Disabled State */}
    <Heading as="h3">{'<Button type="whatever" disabled  />'}</Heading>
    <Flex m="lg">
      <Button type="primary" mr="lg" disabled>
        Primary Button
      </Button>
      <Button type="secondary" mr="lg" disabled>
        Secondary Button
      </Button>
      <Button type="success" mr="lg" disabled>
        Success Button
      </Button>
      <Button type="danger" mr="lg" disabled>
        Danger Button
      </Button>
      <Button type="warning" mr="lg" disabled>
        Warning Button
      </Button>
      <Button type="info" mr="lg" disabled>
        Info Button
      </Button>
    </Flex>
    {/* Spinner  *******************************/}

    <Heading my="md" as="h3">
      {'<Spinner size="sm" />'}
    </Heading>
    <Spinner size="sm" m="lg" />

    <Heading mb="md" as="h3">
      {'<Spinner size="lg" />'}
    </Heading>
    <Spinner size="lg" m="lg" />

    <Heading mb="md" as="h3">
      {'<Spinner size="xl" />'}
    </Heading>
    <Spinner size="xl" m="lg" />

    {/* AlertBox  *******************************/}

    <Heading mb="lg" mt="xl" as="h2">
      Alertbox
    </Heading>

    <Heading as="h3">{'<AlertBox />'}</Heading>
    <AlertBox m="lg">Dummy Text!!</AlertBox>

    <Heading as="h3">{'<AlertBox type="success" />'}</Heading>
    <AlertBox type="success" m="lg">
      Hello Testing!
    </AlertBox>

    <Heading as="h3">{'<AlertBox type="info" />'}</Heading>
    <AlertBox type="info" m="lg" />

    <Heading as="h3">{'<AlertBox type="warning" />'}</Heading>
    <AlertBox type="warning" m="lg" />

    <Heading as="h3">{'<AlertBox type="error" />'}</Heading>
    <AlertBox type="error" m="lg" />

    {/* Tabs *********************************/}

    <Heading mt="xl" mb="lg" as="h2">
      React Tab Component
    </Heading>

    <Heading mb="lg" as="h3">
      {'<Tabs tabsData={array} />'}
    </Heading>
    <Tabs tabsData={dummytabsData} />

    {/* Icons *********************************/}

    <Heading as="h2" mt="xl" mb="lg">
      Icon Component
    </Heading>

    <Heading my="lg" as="h3">
      {'<Icon /> ~ default'}
    </Heading>
    <Icon ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon size={20} /> ~ default size is "14px"'}
    </Heading>
    <Icon ml="xl" size={20} />

    <Heading my="lg" as="h3">
      {'<Icon pointer /> ~ pointer prop'}
    </Heading>
    <Icon ml="xl" pointer />

    <Heading my="lg" as="h3">
      {'<Icon type="success" />'}
    </Heading>
    <Icon type="success" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="success" color="yellow.primary />'}
    </Heading>
    <Icon type="success" color="yellow.primary" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="info" />'}
    </Heading>
    <Icon type="info" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="warning" />'}
    </Heading>
    <Icon type="warning" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="error" />'}
    </Heading>
    <Icon type="error" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="graphiQL" />'}
    </Heading>
    <Icon type="graphiQL" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="database" />'}
    </Heading>
    <Icon type="database" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="schema" />'}
    </Heading>
    <Icon type="schema" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="event" />'}
    </Heading>
    <Icon type="event" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="settings" />'}
    </Heading>
    <Icon type="settings" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="question" />'}
    </Heading>
    <Icon type="question" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="action" />'}
    </Heading>
    <Icon type="action" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="exclamation" />'}
    </Heading>
    <Icon type="exclamation" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="close" />'}
    </Heading>
    <Icon type="close" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="star" />'}
    </Heading>
    <Icon type="star" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="twitter" />'}
    </Heading>
    <Icon type="twitter" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="love" />'}
    </Heading>
    <Icon type="love" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="right" />'}
    </Heading>
    <Icon type="right" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="down" />'}
    </Heading>
    <Icon type="down" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="up" />'}
    </Heading>
    <Icon type="up" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="compress" />'}
    </Heading>
    <Icon type="compress" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="expand" />'}
    </Heading>
    <Icon type="expand" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="edit" />'}
    </Heading>
    <Icon type="edit" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="pencil" />'}
    </Heading>
    <Icon type="pencil" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="search" />'}
    </Heading>
    <Icon type="search" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="spinner" />'}
    </Heading>
    <Icon type="spinner" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="questionCircle" />'}
    </Heading>
    <Icon type="questionCircle" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="pause" />'}
    </Heading>
    <Icon type="pause" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="play" />'}
    </Heading>
    <Icon type="play" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="playBox" />'}
    </Heading>
    <Icon type="playBox" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="check" />'}
    </Heading>
    <Icon type="check" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="reload" />'}
    </Heading>
    <Icon type="reload" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="delete" />'}
    </Heading>
    <Icon type="delete" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="add" />'}
    </Heading>
    <Icon type="add" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="eye" />'}
    </Heading>
    <Icon type="eye" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="secret" />'}
    </Heading>
    <Icon type="secret" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="lightBulb" />'}
    </Heading>
    <Icon type="lightBulb" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="sort" />'}
    </Heading>
    <Icon type="sort" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="caretUp" />'}
    </Heading>
    <Icon type="caretUp" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="caretDown" />'}
    </Heading>
    <Icon type="caretDown" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="caretRight" />'}
    </Heading>
    <Icon type="caretRight" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="caretLeft" />'}
    </Heading>
    <Icon type="caretLeft" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="clone" />'}
    </Heading>
    <Icon type="clone" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="copy" />'}
    </Heading>
    <Icon type="copy" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="link" />'}
    </Heading>
    <Icon type="link" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="table" />'}
    </Heading>
    <Icon type="table" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="filter" />'}
    </Heading>
    <Icon type="filter" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="wrench" />'}
    </Heading>
    <Icon type="wrench" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="send" />'}
    </Heading>
    <Icon type="send" ml="xl" />

    <Heading my="lg" as="h3">
      {'<Icon type="fork" />'}
    </Heading>
    <Icon type="fork" ml="xl" />

    {/* ToolTip ********************************/}
    <Heading mb="lg" mt="xl" as="h2">
      ToolTip Component
    </Heading>

    <Heading mb="lg" mt="xl" as="h3">
      {'<ToolTip />'}
    </Heading>
    <ToolTip message="Dummy Text" ml="xl">
      Hover me!!
    </ToolTip>

    <Heading mb="lg" mt="xl" as="h3">
      {'<ToolTip placement="right" />'}
    </Heading>
    <ToolTip message="Dummy Text" placement="right" ml="xl">
      Hover me!!
    </ToolTip>

    <Heading my="xl" as="h3">
      {'<ToolTip placement="top" />'}
    </Heading>
    <ToolTip message="Primary Button" ml="xl" placement="top">
      <Button type="primary" size="small">
        Hover me!
      </Button>
    </ToolTip>

    <Heading mb="lg" mt="xl" as="h3">
      {'<ToolTip placement="left" />'}
    </Heading>
    <ToolTip message="Dummy Text" placement="left" ml="xl">
      Hover me!!
    </ToolTip>

    <Heading mb="lg" mt="xl" as="h3">
      {'<ToolTip placement="bottom" />'}
    </Heading>
    <ToolTip message="Dummy Text" placement="bottom" ml="xl">
      Hover me!!
    </ToolTip>

    {/* Typography ******************************/}

    {/* Heading */}
    <Heading mb="lg" mt="xl" as="h2">
      Typography
    </Heading>

    <Heading mb="lg" mt="xl" as="h3">
      {'<Heading />'}
    </Heading>
    <Heading>Main Heading</Heading>

    <Heading mb="lg" mt="xl" as="h3">
      {'<Heading as="h2" />'}
    </Heading>
    <Heading as="h2">Subpage title</Heading>

    <Heading mb="lg" mt="xl" as="h3">
      {'<Heading as="h3" />'}
    </Heading>
    <Heading as="h3">Section Header</Heading>

    <Heading mb="lg" mt="xl" as="h3">
      {'<Heading as="h4" />'}
    </Heading>
    <Heading as="h4">Sub section Heading</Heading>

    {/* TextLink */}
    <Heading as="h2" mb="lg" mt="xl">
      {'<TextLink />'}
    </Heading>
    <TextLink>Check it out</TextLink>

    <Heading as="h2" mb="lg" mt="xl">
      {'<TextLink underline />'}
    </Heading>
    <TextLink underline>Check it out</TextLink>

    {/* Text */}
    <Heading mb="lg" mt="xl" as="h2">
      {'<Text />'}
    </Heading>
    <Text>
      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
      tempor incididunt ut labore et dolore magna aliqua. Semper quis lectus
      nulla at volutpat diam ut venenatis. Sed viverra tellus in hac habitasse
      platea dictumst. Id porta nibh venenatis cras. Velit dignissim sodales ut
      eu sem. Turpis cursus in hac habitasse platea dictumst quisque. Integer
      feugiat scelerisque varius morbi enim. Dui accumsan sit amet nulla. Donec
      et odio pellentesque diam volutpat commodo sed. Augue eget arcu dictum
      varius duis at. Nullam vehicula ipsum a arcu cursus vitae. Sapien et
      ligula ullamcorper malesuada proin libero nunc. Nunc congue nisi vitae
      suscipit tellus mauris a diam maecenas.
    </Text>
    <Heading my="md" as="h2" mb="lg" mt="xl">
      {"<Text type='explain' />"}
    </Heading>
    {/* Explainer text */}
    <Text type="explain">
      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
      tempor incididunt ut labore et dolore magna aliqua. Semper quis lectus
      nulla at volutpat diam ut venenatis. Sed viverra tellus in hac habitasse
      platea dictumst. Id porta nibh venenatis cras. Velit dignissim sodales ut
      eu sem. Turpis cursus in hac habitasse platea dictumst quisque. Integer
      feugiat scelerisque varius morbi enim. Dui accumsan sit amet nulla. Donec
      et odio pellentesque diam volutpat commodo sed. Augue eget arcu dictum
      varius duis at. Nullam vehicula ipsum a arcu cursus vitae. Sapien et
      ligula ullamcorper malesuada proin libero nunc. Nunc congue nisi vitae
      suscipit tellus mauris a diam maecenas.
    </Text>
    {/* RadioButton ******************************/}
    <Heading mb="lg" mt="xl" as="h2">
      {'<RadioButton name={id} />'}
    </Heading>
    <RadioButton mb="md" name="ex1">
      Choice 1
    </RadioButton>
    <RadioButton mb="md" name="ex2">
      Choice 2
    </RadioButton>
    {/* Checkbox ******************************/}
    <Heading mb="lg" mt="xl" as="h2">
      {'<Checkbox name={id} />'}
    </Heading>
    <Checkbox mb="md" name="test">
      Option 1
    </Checkbox>
    <Checkbox mb="md" name="test2">
      Option 2
    </Checkbox>
    {/* Switch Button */}
    <Heading mb="lg" mt="xl" as="h2">
      {'<SwitchButton />'}
    </Heading>
    <SwitchButton />
  </React.Fragment>
);
