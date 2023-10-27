import { Decorator } from '@storybook/react';
import React, { ReactNode, useCallback, useEffect, useState } from 'react';
import { MenuContent } from './MenuContent';
import { updateStorybookGlobals } from './storybook-globals';
import { ConsoleTypes, EnvStateArgs, MenuOptions } from './types';
import { createEnvState } from './utils';

export const ConsoleTypeDecorator = (
  args: EnvStateArgs & MenuOptions
): Decorator => {
  const { consoleType, adminSecret, ...menuOptions } = args;
  return Story => (
    <ConsoleTypeWrapper
      args={{ consoleType, adminSecret }}
      menuPlacement={menuOptions.menuPlacement ?? 'bottom'}
    >
      <Story />
    </ConsoleTypeWrapper>
  );
};

const ConsoleTypeWrapper = ({
  args = { consoleType: 'oss', adminSecret: false },
  children,
  menuPlacement = 'bottom',
}: {
  args: EnvStateArgs;
  children: ReactNode;
  menuPlacement?: 'top' | 'bottom';
}) => {
  // a simple boolean to stall rendering children until we've updated the window.__env in the mount effect
  const [render, setRender] = useState(false);

  // this is used to apply a key to the div containing the children so it will force it to re-create if it changes
  const [envArgsState, setEnvArgsState] = useState<EnvStateArgs>(args);

  // this just applies a useable __env object to the window property
  const updateState = useCallback(
    (consoleType?: ConsoleTypes, adminSecret?: boolean) => {
      const state = createEnvState({
        adminSecret: adminSecret ?? args.adminSecret, // fallback to what was passed in via decorator
        consoleType: consoleType ?? args.consoleType, // fallback to what was passed in via decorator
      });

      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      // update the window for anything that relies on that directly
      window.__env = { ...window.__env, ...state };

      // update the storybook globals object that's used via Proxy in the primary globals.ts
      updateStorybookGlobals({
        ...state,
        isAdminSecretSet: !!state.adminSecret,
        hasuraCloudTenantId: state.tenantID,
      });
    },
    [args]
  );

  useEffect(() => {
    // update the window on mount:
    updateState();

    // then, render the content
    setRender(true);
  }, []);

  const [minimized, setMinimized] = useState(true);

  return (
    <>
      <MenuContent
        envArgsState={envArgsState}
        menuPlacement={menuPlacement}
        minimized={minimized}
        handleTriggerClick={() => setMinimized(false)}
        handleAdminSwitchChange={enabled => {
          updateState(envArgsState.consoleType, enabled);
          setEnvArgsState(prev => ({ ...prev, adminSecret: enabled }));
        }}
        handleConsoleTypeChange={option => {
          if (option) {
            updateState(option.value);
            setEnvArgsState(prev => ({
              ...prev,
              consoleType: option.value,
            }));
          }
        }}
        handleMinimizeClick={() => setMinimized(true)}
      />
      {render && (
        // use a key to tell the story to re-create the component if the console type/admin secret change
        <React.Fragment key={JSON.stringify(envArgsState)}>
          {children}
        </React.Fragment>
      )}
    </>
  );
};
