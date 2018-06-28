import globals from 'Globals';

const dataHeaders = {
  'Content-Type': 'application/json',
};
if (globals.accessKey !== null) {
  dataHeaders['X-HASURA-ACCESS-KEY'] = globals.accessKey;
}
export default dataHeaders;
