import React from 'react';

import { Button } from '@/new-components/Button';

interface Props extends React.ComponentProps<'div'> {
  existingRelationship?: boolean;
  /**
   * optional callback function, can be used to get the onComplete event, this could be a onSuccess, or onError event.
   *
   */
  onComplete: (callback: {
    title?: string;
    message?: string;
    type: 'success' | 'error' | 'cancel';
  }) => void;
}

export const FormLayout = ({
  existingRelationship,
  onComplete,
  children,
}: Props) => {
  return (
    <div className="w-full sm:w-9/12 bg-white shadow-sm rounded p-md border border-gray-300 show">
      <div className="flex items-center mb-md">
        <Button size="sm" onClick={() => onComplete({ type: 'cancel' })}>
          Cancel
        </Button>
        <span className="font-semibold text-muted ml-1.5">
          {existingRelationship
            ? 'Edit Relationship'
            : 'Create New Relationship'}
        </span>
      </div>
      <hr className="mb-md border-gray-300" />
      <div>{children}</div>
    </div>
  );
};
