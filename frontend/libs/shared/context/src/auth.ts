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
  getHeaders: () => Promise<Record<string, string>>;
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
  logout: () => {},
});

export const useAuthContext = () =>
  useContext(AuthContext) as AuthService<AdminSecretState>;
