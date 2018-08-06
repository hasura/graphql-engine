import { push } from 'react-router-redux';

import globals from '../../../Globals';

const urlPrefix = globals.urlPrefix;
const appPrefix = urlPrefix !== '/' ? urlPrefix + '/data' : '/data';

const _push = path => push(appPrefix + path);

export default _push;
export { appPrefix };
