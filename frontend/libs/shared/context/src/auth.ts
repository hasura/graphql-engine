import { createContext, useContext } from 'react';

export type AdminSecretState = {
  type: 'admin-secret';
  adminSecret: string;
  shouldPersist: boolean;
};

export type AuthService<S, AT = 'none' | 'admin-secret'> = {
  isAuthenticated: boolean;
  authType: AT;
  hasuraUserId?: string;
  // Hasura Cloud collaborator privileges. Undefined means full access.
  privileges?: string[];
  getHeaders: () => Promise<Record<string, string>>;
  // When the credentials returned by getHeaders expire, in epoch milliseconds.
  // Undefined for credentials that don't expire (none, admin secret, PAT).
  sessionExpiry?: number;
  authenticate: (input: S) => Promise<boolean>;
  initialize: () => Promise<void>;
  logout: () => void;
};

export const AuthContext = createContext<AuthService<any>>({
  isAuthenticated: true,
  getHeaders: async () => ({}),
  authType: 'none',
  authenticate: () => Promise.resolve(true),
  initialize: () => Promise.resolve(),
  logout: () => {
    /* noop */
  },
});

export const useAuthContext = () =>
  useContext(AuthContext) as AuthService<AdminSecretState>;
