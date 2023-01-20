import * as React from 'react';

import { IconTooltip } from '@/new-components/Tooltip';
import { Collapsible } from '@/new-components/Collapsible';

interface CollapsibleFieldWrapperProps {
  inputFieldName: string;
  label: string;
  tooltip: string;
}

/**
 * At the time of writing:
 * 1. the FieldWrapper allows adding the label, and the tooltip in a consistent way
 * 2. The Collapsible component allows collapsing a section
 * but none of them allow having a collapsible FieldWrapper. Hence this component brings some parts
 * of the FieldWrapper and mix them with the Collapsible component.
 *
 * TODO: If this pattern will be used more often, we should uniform this behavior.
 * TODO: Fix the a11y issue for which a button cannot be child of another button (speaking about the
 * tooltip trigger being a child of the collapsible trigger)
 */
export const CollapsibleFieldWrapper: React.FC<CollapsibleFieldWrapperProps> =
  props => {
    const { inputFieldName, label, tooltip, children } = props;

    return (
      <Collapsible
        triggerChildren={
          <label
            htmlFor={inputFieldName}
            className="block pt-1 text-gray-600 mb-xs pr-8 flex-grow220px"
          >
            <span className="flex items-center font-semibold">
              <span>{label}</span>
              <span className="font-normal ml-1">(Optional)</span>
              <IconTooltip message={tooltip} />
            </span>
          </label>
        }
      >
        {children}
      </Collapsible>
    );
  };
