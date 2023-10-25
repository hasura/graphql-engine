import clsx from 'clsx';
import { CgSpinner } from 'react-icons/cg';

export const CenteredSpinner: React.VFC<{
  withMiniBackdrop?: boolean;
  position?: 'fixed' | 'absolute';
}> = ({ position = 'fixed', withMiniBackdrop = false }) => (
  <div
    className={clsx(
      'top-1/4 left-1/2 transform -translate-x-1/2 -translate-y-1/2',
      position === 'fixed' ? 'fixed' : 'absolute',
      withMiniBackdrop && 'rounded-xl p-6 z-10 bg-slate-600/5 backdrop-blur-sm'
    )}
  >
    <CgSpinner className="animate-spin" size={30} />
  </div>
);
