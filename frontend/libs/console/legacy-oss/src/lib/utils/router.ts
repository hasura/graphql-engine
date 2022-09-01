import { LocationDescriptor, LocationDescriptorObject } from 'history';

export type onEnterHook = (
  nextState: { location: LocationDescriptorObject },
  replaceState: (location: LocationDescriptor) => void,
  cb?: () => void
) => void;

/**
 * The function makes a wrapper 'onEnter' function which iterates over all the hooks which are passed to it.
 * A hook function can have upto 3 arguments. If the third argument is not supplied the code assumes the
 * hook function is synchronous and would call that after the function.
 *
 * @param  {Array[onEnterHook]} An array of functions which needs to be called onEnter.
 * @return {function}       A function which wraps all the other hooks function.
 */

export const composeOnEnterHooks = (hooks: Array<onEnterHook>) => {
  // The below is the wrapper function which will be given back to the call.
  return (
    nextState: { location: LocationDescriptorObject },
    replaceState: (location: LocationDescriptor) => void,
    finalCallback: () => void
  ) => {
    // Internal recursive function which will be called with all the hooks, or a subset of the hooks.
    const executeRemainingHooks = (remainingHooks: Array<onEnterHook>) => {
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
      return undefined;
    };
    // initial execute with all the hooks.
    executeRemainingHooks(hooks);
  };
};
