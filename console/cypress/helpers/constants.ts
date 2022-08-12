export const ADMIN_SECRET_HEADER_KEY = 'x-hasura-admin-secret';

// TODO cypress default timeout is 4000, we can remove this `AWAIT_SHORT` after verifying that this is followed by a test command that works with timeout
// https://docs.cypress.io/guides/references/configuration#Timeouts
export const AWAIT_SHORT = 2000;
export const AWAIT_MODERATE = 5000;
export const AWAIT_LONG = 7000;
