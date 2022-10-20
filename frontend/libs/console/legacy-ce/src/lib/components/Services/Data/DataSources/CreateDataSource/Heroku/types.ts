export type HerokuSession = {
  access_token: string;
  expires_in: number;
  refresh_token: string;
  token_type: string;
};

export type HerokuConfigVars = {
  DATABASE_URL: string;
};

export type HerokuError = {
  message: string;
};

export type HerokuApp = {
  name: string;
  id: string;
};

export type HerokuAccount = {
  name: string;
  email: string;
};

export type HerokuSessionData = {
  herokuTokenExchange: HerokuSession;
};

export type HerokuExchangeVars = {
  code: string;
};

export type HerokuRefreshVars = {
  refreshToken: string;
};

export type StateDetails<ProgressDetails = undefined> =
  | {
      status: 'pending';
    }
  | {
      status: 'in-progress';
    }
  | {
      status: 'success';
      details: ProgressDetails;
    }
  | {
      status: 'failed';
      details: HerokuError;
    };

export type ProgressState = {
  'creating-app': StateDetails<HerokuApp>;
  'installing-postgres': StateDetails;
  'getting-config': StateDetails<HerokuConfigVars>;
};
