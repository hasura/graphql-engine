import React, { useEffect } from 'react';

type HandledEvents = ['mousedown', 'touchstart'];
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
export function useOnClickOutside(
  refs: React.RefObject<HTMLElement | null>[],
  handler: Handler,
) {
  useEffect(() => {
    const listener = (event: PossibleEvent) => {
      // Do nothing if clicking ref's element or descendent elements.
      // Refs that aren't currently mounted (ref.current is null) are
      // ignored rather than treated as a match, so they never block the
      // handler from firing.
      const clickedInsideARef = refs.some(
        (ref) => ref.current && ref.current.contains(event.target as Node),
      );

      if (clickedInsideARef) {
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
}
