import * as React from 'react';

const MOUSEDOWN = 'mousedown';
const TOUCHSTART = 'touchstart';
type HandledEvents = [typeof MOUSEDOWN, typeof TOUCHSTART];
type HandledEventsType = HandledEvents[number];
type PossibleEvent = {
  [Type in HandledEventsType]: HTMLElementEventMap[Type];
}[HandledEventsType];
type Handler = (event: PossibleEvent) => void;

/**
 * useOnClickOutside hook takes a list refs to track and runs the handler if the click occurs outside of the refs
 * @param refs
 * @param handler
 */
export default function useOnClickOutside(
  refs: Array<React.RefObject<HTMLElement>>,
  handler: Handler
) {
  React.useEffect(() => {
    const listener = (event: PossibleEvent) => {
      // Do nothing if clicking ref's element or descendent elements
      const refsClicked = refs.filter(
        ref => !ref.current || ref.current.contains(event.target as Node)
      );

      if (refsClicked.length) {
        return;
      }

      handler(event);
    };
    document.addEventListener('mousedown', listener);
    document.addEventListener('touchstart', listener);
    return () => {
      document.removeEventListener('mousedown', listener);
      document.removeEventListener('touchstart', listener);
    };
  }, [refs, handler]);
  // ... but to optimize you can wrap handler in useCallback before ...
  // ... callback/cleanup to run every render ...
  // ... function on every render that will cause this effect ...
}
