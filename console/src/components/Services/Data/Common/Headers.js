import globals from 'Globals';

const dataHeaders = {
  'Content-Type': 'application/json',
};
if (
  globals.accessKey !== null &&
  globals.accessKey !== '' &&
  globals.accessKey !== undefined
) {
  dataHeaders['X-HASURA-ACCESS-KEY'] = globals.accessKey;
}
export default dataHeaders;
