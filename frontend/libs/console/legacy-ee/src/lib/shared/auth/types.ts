import { Privilege } from '@hasura/console-legacy-ce';
import { AuthService } from '@hasura/shared/context';

export type AuthType = 'none' | 'admin-secret' | 'pat' | 'sso' | 'hasura-sso';

export type LuxProjectInfo = {
  id?: string;
  name?: string;
  privileges?: Privilege[];
  metricsFQDN?: string;
  plan_name?: string;
  entitlements?: any[];
};

export type SSOAuthState = {
  accessToken: string;
  idToken: string;
  refreshToken?: string;
  expiry?: string;
};

export type EnterpriseAuthState =
  | { type: 'none' }
  | {
      type: 'admin-secret';
      adminSecret: string;
      shouldPersist: boolean;
    }
  | {
      type: 'pat';
      pat: string;
    }
  | (SSOAuthState & {
      type: 'sso';
      clientId: string;
    })
  | (SSOAuthState & {
      type: 'hasura-sso';
      userId?: string;
      project: LuxProjectInfo | null;
    });

export type EnterpriseAuthService = AuthService<
  EnterpriseAuthState,
  AuthType
> & {
  privileges: Privilege[];
  getMetricsHeaders: () => Promise<Record<string, string>>;
};
