import React from 'react';

type Props = {
  className: string;
};

export function CustomRightChevron(props: Props) {
  const { className } = props;
  return (
    <svg
      className={className}
      viewBox="0 0 22 80"
      fill="none"
      preserveAspectRatio="none"
    >
      <path
        d="M0 -2L20 40L0 82"
        vectorEffect="non-scaling-stroke"
        stroke="currentcolor"
        strokeLinejoin="round"
      />
    </svg>
  );
}
