import { push } from 'react-router-redux';

import globals from '../../../Globals';

const urlPrefix = globals.urlPrefix;
const appPrefix = urlPrefix !== '/' ? urlPrefix + '/events' : '/events';

const _push = path => push(appPrefix + path);

export default _push;
export { appPrefix };
