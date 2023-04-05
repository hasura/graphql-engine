import React from 'react';

type Props = {
  label: string;
};

export function ListHeader(props: Props) {
  const { label } = props;
  return (
    <div className="px-xs border-b pb-xs text-muted text-sm font-semibold">
      {label}
    </div>
  );
}
