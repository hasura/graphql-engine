import { push } from 'react-router-redux';

import globals from '../../../Globals';

const urlPrefix = globals.urlPrefix;
const rootPrefix = urlPrefix !== '/' ? urlPrefix : '';
const appPrefix = rootPrefix + '/data';

const _push = path => push(rootPrefix + path);

export default _push;
export { appPrefix };
