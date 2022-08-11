import React from 'react';
import { Provider } from 'react-redux';
import produce from 'immer';
import addons from '@storybook/addons';
import { DocsContainer, DocsPage } from '@storybook/addon-docs';
import { initialize, mswDecorator } from 'msw-storybook-addon';
import theme from './theme';
import '../src/theme/tailwind.css';
import { store } from '../src/store';
import '../src/components/Common/Common.module.scss';
import { DARK_MODE_EVENT_NAME } from 'storybook-dark-mode';

const channel = addons.getChannel();
initialize();

export const parameters = {
  actions: { argTypesRegex: '^on.*' },
  options: {
    storySort: {
      order: ['Design system', 'Dev', 'Components', 'Hooks'],
    },
  },
  controls: {
    matchers: {
      color: /(background|color)$/i,
      date: /Date$/,
    },
  },
  docs: {
    container: props => {
      // Sync Docs dark mode with Storybook Manager
      const [isDark, setDark] = React.useState();

      React.useEffect(() => {
        channel.on(DARK_MODE_EVENT_NAME, setDark);
        return () => channel.removeListener(DARK_MODE_EVENT_NAME, setDark);
      }, [channel, setDark]);

      return (
        <div className={(isDark ? 'dark' : 'light') + ' hasura-tailwind-on'}>
          <DocsContainer
            {...props}
            context={{
              ...props.context,
              storyById: id => {
                return produce(props.context.storyById(id), draft => {
                  draft.parameters.docs.theme = isDark
                    ? theme.dark
                    : theme.light;
                });
              },
            }}
          >
            {props.children}
          </DocsContainer>
        </div>
      );
    },
    page: DocsPage,
  },
  darkMode: {
    dark: { ...theme.dark },
    light: { ...theme.light },
  },
};

export const decorators = [
  (fn, c) => <Provider store={store}>{fn(c)}</Provider>,
  mswDecorator,
  Story => {
    document.body.classList.add('hasura-tailwind-on');
    return <div className={'bg-legacybg'}>{Story()}</div>;
  },
];

export const argTypes = {
  disableSnapshotTesting: {
    table: {
      disable: true,
    },
  },
};
