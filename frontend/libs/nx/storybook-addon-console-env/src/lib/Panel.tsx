import React, { useMemo, useCallback, useEffect } from 'react';
import {
  Args,
  ArgTypes,
  useGlobals,
  useGlobalTypes,
  useParameter,
} from '@storybook/api';
import { AddonPanel } from '@storybook/components';
import { ADDON_ID } from './constants';
import { PureArgsTable } from '@storybook/blocks';

type PanelProps = {
  active?: boolean;
};

const filterUncontrolledTypes = (globalTypes: ArgTypes) =>
  Object.entries(globalTypes).reduce((acc, [key, arg]) => {
    if (arg['control'] !== undefined) acc[key] = arg;
    return acc;
  }, {} as ArgTypes);

const getRows = (globalTypes: ArgTypes) => filterUncontrolledTypes(globalTypes);

export const Panel = ({ active = true }: PanelProps) => {
  const globalTypes = useGlobalTypes();
  const [globals, updateGlobals] = useGlobals();
  const adminSecretSetParameter = useParameter(
    'adminSecretSet',
    globalTypes['adminSecretSet']?.defaultValue
  );
  const consoleTypeParameter = useParameter(
    'consoleType',
    globalTypes['consoleType']?.defaultValue
  );

  const refreshAndUpdateGlobal = useCallback(
    (newGlobals: Args) => {
      updateGlobals(newGlobals);
      setTimeout(() => {
        // Force reload iFrame
        (
          document.getElementById(
            'storybook-preview-iframe'
          ) as HTMLIFrameElement
        ).contentDocument?.location.reload();
      });
    },
    [updateGlobals]
  );

  useEffect(() => {
    if (
      (adminSecretSetParameter !== null &&
        adminSecretSetParameter !== globals['adminSecretSet']) ||
      (consoleTypeParameter !== null &&
        consoleTypeParameter !== globals['consoleType'])
    ) {
      const newGlobals = { ...globals };
      console.log('newGlobals', newGlobals);
      if (adminSecretSetParameter !== null) {
        newGlobals['adminSecretSet'] = adminSecretSetParameter;
      }
      if (consoleTypeParameter !== null) {
        newGlobals['consoleType'] = consoleTypeParameter;
      }
      refreshAndUpdateGlobal(newGlobals);
    }
  }, [adminSecretSetParameter, consoleTypeParameter]);

  const rows = useMemo(() => getRows(globalTypes), [globalTypes]);

  return (
    <AddonPanel active={active} key={ADDON_ID}>
      {Object.values(rows).length > 0 ? (
        <PureArgsTable
          inAddonPanel
          rows={rows}
          args={globals}
          updateArgs={refreshAndUpdateGlobal}
        />
      ) : (
        <span>
          Please add global types (with <code>control</code> property and the
          needed JS code in <code>preview-body.html</code>)
        </span>
      )}
    </AddonPanel>
  );
};
