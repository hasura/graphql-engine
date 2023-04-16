// SSO login identity configs that enable Single Sign-on
// integration with external OAuth providers for EE customer
// read more at https://github.com/hasura/lux/blob/main/docs/rfcs/20230214_enterprise_sso_jwt.md
export type SsoIdentityProvider = {
  client_id: string;
  name: string;
  scope: string;
  authorization_url: string;
  request_token_url: string;
};

export type SsoIdentityProviders = SsoIdentityProvider[];
