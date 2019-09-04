/**
 * The function makes a wrapper 'onEnter' function which iterates over all the hooks which are passed to it.
 * A hook function can have upto 3 arguments. If the third argument is not supplied the code assumes the
 * hook function is synchronous and would call that after the function.
 *
 * @param  {Array[function]} An array of functions which needs to be called onEnter.
 * @return {function}       A function which wraps all the other hooks function.
 */
export const composeOnEnterHooks = hooks => {
  // The below is the wrapper function which will be given back to the call.
  return (nextState, replaceState, finalCallback) => {
    // Internal recursive function which will be called with all the hooks, or a subset of the hooks.
    const executeRemainingHooks = remainingHooks => {
      // if I got no more hooks then call the finalCallback
      if (remainingHooks.length === 0) {
        return finalCallback();
      }
      const nextHook = remainingHooks[0];
      // if the function has 3 or more argument then it assumes the 3 argument is a callback to the next hook.
      if (nextHook.length >= 3) {
        nextHook.call(this, nextState, replaceState, () => {
          // subset of hooks with the current hook removed.
          executeRemainingHooks(remainingHooks.slice(1));
        });
      } else {
        // the other block
        nextHook.call(this, nextState, replaceState);
        executeRemainingHooks(remainingHooks.slice(1));
      }
    };
    // initial execute with all the hooks.
    executeRemainingHooks(hooks);
  };
};

/**
 * Return a function which takes (location, callback). This is compatible with getComponent attribute
 * of react-router 'Router component'. The function help with webpack to split the main bundle to multiple
 * chuck which reduces initial load-time.
 * FIXME: THIS DOESN'T WORK. Need to find a better way - best option now is to upgrade to webpack2 (But it not a trivial change).
 *
 * @param  {String} componentPath the path of the component in the system. The component is expected to have a default.
 * @return {function}               the closure function which is used.
 */
export const asyncComponent = componentPath => {
  const asyncRoutFunc = `(function (location, callback) {
    require.ensure([], (function (require) {
      callback(null, require("${componentPath}"));
    }));
  });`;
  /* eslint-disable no-eval */
  return eval(asyncRoutFunc);
};
