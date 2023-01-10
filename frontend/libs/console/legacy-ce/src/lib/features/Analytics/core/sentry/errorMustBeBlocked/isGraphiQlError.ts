import type { ErrorInfo } from './errorMustBeBlocked';

/**
 * Identify the GraphiQl errors we do not want to hit Sentry.
 * Unfortunately, the 1.0.0-alpha.0 version of GraphiQL (the one we use at the time of writing) does
 * not expose APIs to manage the errors. The best thing that we can do is trying to identify
 * and block them downstream.
 *
 * Please note that we must be extremely careful when blocking errors, false positives are better
 * then not being notified at all!
 */
export function isGraphiQlError(info: ErrorInfo) {
  const { error, urlPrefix, pathname } = info;
  const errorMessage = typeof error === 'string' ? error : error?.message;

  if (!errorMessage) return false;

  // Please note that the "/"  pathname should not be blocked, because only while working locally,
  // the console is served on "/" despite of the URL_PREFIX env variable.
  const isApiExplorer =
    // see: https://sentry.io/organizations/hasura-nn/issues/3671973903/tags/url/?project=6684052
    pathname === urlPrefix ||
    // see: https://sentry.io/organizations/hasura-nn/issues/3672788747/tags/url/?project=6684052
    pathname === `${urlPrefix}/` ||
    // see: https://sentry.io/organizations/hasura-nn/issues/3671973903/tags/url/?project=6684052
    pathname.includes('/api/api-explorer');

  if (!isApiExplorer) return false;

  // An example of the GraphiQL error we want to block:
  // Name "__twoDashesColumn" must not begin with "__", which is reserved by GraphQL introspection.
  //
  // If you want to reproduce it locally:
  // 1. Create a table named "__twoSlashes"
  // 2. Run the following query in GraphiQL
  // query MyQuery {
  //   __twoSlashes
  // }
  // 3. Look at the error in the browser console
  if (errorMessage.includes('which is reserved by GraphQL introspection')) {
    return true;
  }

  // An example of the GraphiQL error we want to block:
  // Input Object type msgbox_UserInbox_stream_cursor_value_input must define one or more fields.
  // see: https://sentry.io/organizations/hasura-nn/issues/3699191288/
  if (
    errorMessage.includes('Input Object type') &&
    errorMessage.includes('must define one or more fields.')
  ) {
    return true;
  }

  // see: https://sentry.io/organizations/hasura-nn/issues/3671973903
  if (
    errorMessage ===
    `Cannot read properties of undefined (reading 'variableDefinitions')`
  ) {
    return true;
  }

  // see: https://sentry.io/organizations/hasura-nn/issues/3703083666/
  if (
    errorMessage === `Cannot read properties of undefined (reading 'getFields')`
  ) {
    return true;
  }

  return false;
}
