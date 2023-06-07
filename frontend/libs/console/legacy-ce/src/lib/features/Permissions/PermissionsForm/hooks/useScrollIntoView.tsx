import { useEffect } from 'react';

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

const useScrollIntoView = (
  ref: React.RefObject<any>,
  deps: string[],
  options: ScrollIntoViewOptions | boolean = {}
) => {
  useEffect(() => {
    if (ref && ref?.current?.scrollIntoView) {
      ref.current.scrollIntoView(options);
    }
  }, [...deps, ref]);
};

export default useScrollIntoView;
