import { parse as gql } from 'graphql';
export const eeApiHeaders = {};
export const EE_LICENSE_INFO_QUERY_NAME = 'EE_LICENSE_INFO_QUERY_NAME';
export const LICENSE_REFRESH_INTERVAL = 3600000;
export const EE_TRIAL_DOCS_URL =
  'https://hasura.io/docs/latest/enterprise/try-hasura-enterprise-edition';

/**
 * GraphQl mutation to register the user for EE trial
 */
export const REGISTER_EE_TRIALS_MUTATION = gql(`
  mutation registerEETrial(
        $first: String!
        $last: String!
        $email: String!
        $jobFunction: String!
        $organization: String!
        $phone: String!
        $password: String!
        $hasuraUseCase: String!
        $eeUseCase: [String]!

  ) {
     registerEETrial(
        first: $first,
        last: $last,
        email: $email,
        jobFunction: $jobFunction,
        organization: $organization,
        phone: $phone
        password: $password
        hasuraUseCase: $hasuraUseCase
        eeUseCase: $eeUseCase
    ){
      client_id
      client_secret
    }
  }
`);

/**
 * GraphQl mutation to activate an existing EE trial license
 */
export const ACTIVATE_EE_TRIALS_MUTATION = gql(`
  mutation registerEETrial(
    $email: String!
    $password: String!
  ) {
     registerEETrial(
        email: $email
        password: $password
    ){
      client_id
      client_secret
    }
  }
`);

export const EE_TRIAL_CONTACT_US_URL =
  'https://hasurahelp.zendesk.com/hc/en-us/requests/new';
