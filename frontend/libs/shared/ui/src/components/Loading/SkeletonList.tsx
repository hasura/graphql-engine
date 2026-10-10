import { Skeleton, SkeletonProps } from '@radix-ui/themes';
import clsx from 'clsx';

export type SkeletonListProps = SkeletonProps & {
  count: number;
  containerClassName?: string;
};

export const SkeletonList = ({
  containerClassName,
  count,
  height = '20px',
  width = '100%',
  className,
  ...others
}: SkeletonListProps) => {
  return (
    <div className={containerClassName}>
      {Array.from({
        length: count,
      }).map((_, i) => (
        <Skeleton
          key={i}
          width={width}
          height={height}
          className={clsx('mb-2', className)}
          {...others}
        />
      ))}
    </div>
  );
};
