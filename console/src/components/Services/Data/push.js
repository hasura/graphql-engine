import { push } from 'react-router-redux';
import globals from '../../../Globals';

const appPrefix = globals.urlPrefix + '/data';

const _push = path => push(appPrefix + path);

export default _push;
export { appPrefix };
