import { z } from 'zod';
import React, { createContext } from 'react';
import { devtools } from 'zustand/middleware';
import { createStore, useStore as useZustandStore } from 'zustand';

// --------------------------------------------------
// TYPES
// --------------------------------------------------

export type State = {
  hasuraPlan: HasuraPlan;
  serverEnvVars: ServerEnvVars;
};

type Setters = {
  setHasuraPlan: (hasuraPlan: HasuraPlan) => void;
  setEeLicense: (eeLicense: EeLicense) => void;
  setServerEnvVars: (envVars: ServerEnvVars) => void;
};

export type Store = State & Setters;

/**
 * All the info about the current Hasura plan.
 *
 * If you are experienced with the window.__env usage, think of this object as a computed object
 * that includes all the info the Console previously tried to gather from the various window.__env
 * combinations.
 *
 * All the missing plans are not supported yet.
 */
export type HasuraPlan =
  | {
      // The plan info still needs to be fetched. This happens when
      // - the result of the `version` API is not available yet
      // - the plan is not managed yet by the feature-first API
      name: 'unknown';
    }
  | {
      name: 'ce';
    }
  | {
      name: 'ee';
      license:
        | {
            // The license info still need to be fetched.
            status: 'unknown';
          }
        | {
            // The license API failed to respond
            status: 'licenseApiError';
          }
        | {
            // Corresponds to the EELicenseInfo['status'] == 'none'.
            status: 'missing';
          }
        | {
            status: 'active';
            expiresAt: Date;
            type: EeLicenseType;
          }
        | {
            status: 'grace';
            expiresAt: Date;
            type: EeLicenseType;
          }
        | {
            status: 'expired';
            expiredAt?: Date;
            type: EeLicenseType;
          }
        | {
            status: 'deactivated';
            type: EeLicenseType;
          };
    }
  | {
      name: 'cloud';
    }
  | {
      name: 'eeClassic';
    };

type EeLicenseType = 'trial' | 'paid';

type EeLicense = Extract<HasuraPlan, { name: 'ee' }>['license'];

const serverEnvVarsSchema = z.object({
  consoleType: z
    .union([
      z.literal('oss'),
      z.literal('pro'),
      z.literal('cloud'),
      z.literal('pro-lite'),
      z.literal('ee-classic'),
    ])
    // The CLI web server does not pass the consoleType
    .optional(),
});

export type ServerEnvVars = z.infer<typeof serverEnvVarsSchema>;

// --------------------------------------------------
// STORE
// --------------------------------------------------
const defaultState: State = {
  hasuraPlan: { name: 'unknown' },
  serverEnvVars: {},
};

function createNewStore() {
  const consoleInfoStore = createStore<Store>()(
    devtools(
      set => ({
        ...defaultState,

        setServerEnvVars: serverEnvVars =>
          set(
            prev => ({
              ...prev,
              serverEnvVars,
            }),
            false,
            {
              type: 'setServerEnvVars',
              serverEnvVars,
            }
          ),

        setHasuraPlan: hasuraPlan =>
          set(
            prev => ({
              ...prev,
              hasuraPlan,
            }),
            false,
            {
              type: 'setHasuraPlan',
              hasuraPlan,
            }
          ),

        setEeLicense: eeLicense =>
          set(
            prev => {
              if (prev.hasuraPlan.name !== 'ee') {
                console.error(
                  `The EE license cannot be set on a ${prev.hasuraPlan.name} plan`
                );
                return prev;
              }

              return {
                ...prev,
                hasuraPlan: {
                  ...prev.hasuraPlan,
                  license: eeLicense,
                },
              };
            },
            false,
            {
              type: 'setEeLicense',
              eeLicense,
            }
          ),
      }),
      // Assign a name to the store for debugging purposes
      { name: 'ConsoleInfoStore' }
    )
  );

  return consoleInfoStore;
}

// --------------------------------------------------
// CONTEXT
// --------------------------------------------------

const useStoreValue = () => {
  return createNewStore();
};

export type StoreContextType = ReturnType<typeof useStoreValue>;

const UseStore = createContext<StoreContextType | undefined>(undefined);

export const StoreProvider = ({ children }: { children: React.ReactNode }) => {
  const value = useStoreValue();

  return <UseStore.Provider value={value}>{children}</UseStore.Provider>;
};

export const MockStoreContextProvider = (
  props: React.PropsWithChildren<{
    params: StoreContextType;
  }>
) => {
  return <UseStore.Provider value={props.params} {...props} />;
};

export const useStore = () => {
  const context = React.useContext(UseStore);
  if (context === undefined) {
    throw new Error('useCount must be used within a StoreProvider');
  }
  return context;
};

// --------------------------------------------------
// REACT APIS
// --------------------------------------------------

export function useHasuraPlan() {
  const store = useStore();
  return useZustandStore(store, state => state.hasuraPlan);
}

export function useSetHasuraPlan() {
  const store = useStore();
  return useZustandStore(store, state => state.setHasuraPlan);
}

export function useSetEeLicense() {
  const store = useStore();
  return useZustandStore(store, state => state.setEeLicense);
}

export function useSetServerEnvVars() {
  const store = useStore();
  return useZustandStore(store, state => state.setServerEnvVars);
}
