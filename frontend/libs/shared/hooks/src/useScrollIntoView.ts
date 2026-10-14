import { useEffect, useRef } from 'react';

export enum ScrollIntoViewBehavior {
  auto = 'auto',
  smooth = 'smooth',
}

export enum ScrollIntoViewBlock {
  start = 'start',
  center = 'center',
  end = 'end',
  nearest = 'nearest',
}

export enum ScrollIntoViewInline {
  start = 'start',
  center = 'center',
  end = 'end',
  nearest = 'nearest',
}

export type ScrollIntoViewOptions = {
  behavior?: keyof typeof ScrollIntoViewBehavior;
  block?: keyof typeof ScrollIntoViewBlock;
  inline?: keyof typeof ScrollIntoViewInline;
};

export const useScrollIntoView = (
  ref: React.RefObject<any>,
  deps: string[],
  options: ScrollIntoViewOptions | boolean = {},
) => {
  // Keep the latest `options` in a ref so the scroll effect can read the
  // current value without having to re-run when a caller passes a freshly
  // created options object on every render. The effect is intentionally driven
  // by the caller-supplied `deps` (and `ref`), mirroring the original API.
  const optionsRef = useRef(options);
  useEffect(() => {
    optionsRef.current = options;
  });

  // `deps` is a caller-controlled dependency list. Serialize it to a stable
  // primitive so the effect re-runs exactly when the dep values change, which
  // preserves the previous `[...deps, ref]` behavior while remaining
  // statically verifiable by the linter.
  const depsKey = JSON.stringify(deps);
  useEffect(() => {
    if (ref && ref?.current?.scrollIntoView) {
      ref.current.scrollIntoView(optionsRef.current);
    }
  }, [depsKey, ref]);
};
