import React from 'react';

type EmptyStateProps = {
  title: string;
  subtitle: string;
  buttons: React.ReactElement[];
};

export const EmptyState = ({ title, subtitle, buttons }: EmptyStateProps) => {
  return (
    <div className="justify-center text-center">
      <p className="mb-1">{title}</p>
      <p className="font-bold mb-sm">{subtitle}</p>
      <div>{buttons}</div>
    </div>
  );
};
