import React, { useMemo, useCallback, useEffect, useRef } from 'react';
import {
  useGlobals,
  useGlobalTypes,
  useParameter,
} from 'storybook/manager-api';
import type { Args, ArgTypes } from 'storybook/internal/types';
import { AddonPanel } from 'storybook/internal/components';
import { ADDON_ID } from './constants';
import { PureArgsTable } from '@storybook/addon-docs/blocks';

type PanelProps = {
  active?: boolean;
};

const filterUncontrolledTypes = (globalTypes: ArgTypes) =>
  Object.entries(globalTypes).reduce((acc, [key, arg]: [string, any]) => {
    if (arg['control'] !== undefined) acc[key] = arg;
    return acc;
  }, {} as ArgTypes);

const getRows = (globalTypes: ArgTypes) => filterUncontrolledTypes(globalTypes);

export const Panel = ({ active = true }: PanelProps) => {
  const globalTypes = useGlobalTypes();
  const [globals, updateGlobals] = useGlobals();
  const adminSecretSetParameter = useParameter(
    'adminSecretSet',
    globalTypes['adminSecretSet']?.defaultValue,
  );
  const consoleTypeParameter = useParameter(
    'consoleType',
    globalTypes['consoleType']?.defaultValue,
  );

  const refreshAndUpdateGlobal = useCallback(
    (newGlobals: Args) => {
      updateGlobals(newGlobals);
      setTimeout(() => {
        // Force reload iFrame
        (
          document.getElementById(
            'storybook-preview-iframe',
          ) as HTMLIFrameElement
        ).contentDocument?.location.reload();
      });
    },
    [updateGlobals],
  );

  // This effect pushes the active story's parameters into Storybook globals and
  // must run ONLY when those parameters change. `globals` and
  // `refreshAndUpdateGlobal` are read via a ref holding their latest values so
  // the effect does not re-run on every globals change (which would revert
  // manual edits made through the args table back to the story parameters).
  const latestRef = useRef({ globals, refreshAndUpdateGlobal });
  // Keep the ref pointed at the latest values. Declared before the sync effect
  // below so, on every commit, the ref is refreshed first and the sync effect
  // reads current values.
  useEffect(() => {
    latestRef.current = { globals, refreshAndUpdateGlobal };
  });

  useEffect(() => {
    const { globals: currentGlobals, refreshAndUpdateGlobal: refresh } =
      latestRef.current;
    if (
      (adminSecretSetParameter !== null &&
        adminSecretSetParameter !== currentGlobals['adminSecretSet']) ||
      (consoleTypeParameter !== null &&
        consoleTypeParameter !== currentGlobals['consoleType'])
    ) {
      const newGlobals = { ...currentGlobals };
      console.log('newGlobals', newGlobals);
      if (adminSecretSetParameter !== null) {
        newGlobals['adminSecretSet'] = adminSecretSetParameter;
      }
      if (consoleTypeParameter !== null) {
        newGlobals['consoleType'] = consoleTypeParameter;
      }
      refresh(newGlobals);
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
