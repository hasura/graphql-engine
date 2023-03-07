import { Button } from '../../../../new-components/Button';
import { Analytics } from '../../../Analytics';
import clsx from 'clsx';
import React from 'react';
import { FaChevronDown, FaExternalLinkAlt } from 'react-icons/fa';
import { Operation } from './types';
import globals from '../../../../Globals';

export interface OasGeneratorActionsProps {
  operation: Operation;
  existing?: boolean;
  onCreate: () => void;
  onDelete: () => void;
  disabled?: boolean;
}

export const OasGeneratorActions: React.FC<
  OasGeneratorActionsProps
> = props => {
  const { operation, existing, onCreate, onDelete, disabled } = props;
  const [isExpanded, setExpanded] = React.useState(false);
  return (
    <div data-testid={`operation-${operation.operationId}`}>
      <div className="flex justify-between cursor-pointer">
        <div className="max-w-[17vw] overflow-hidden truncate">
          {operation.path}
        </div>
        {existing ? (
          <div className="flex items-center space-x-xs -my-2">
            <Analytics
              name="action-tab-btn-import-openapi-delete-action"
              passHtmlAttributesToChildren
            >
              <Button
                disabled={disabled}
                size="sm"
                mode="destructive"
                onClick={onDelete}
              >
                Delete
              </Button>
            </Analytics>
            <Analytics
              name="action-tab-btn-import-openapi-modify-action"
              passHtmlAttributesToChildren
            >
              <Button
                icon={<FaExternalLinkAlt />}
                iconPosition="end"
                disabled={disabled}
                size="sm"
                onClick={e => {
                  window.open(
                    `${globals.urlPrefix}/actions/manage/${operation.operationId}/modify`,
                    '_blank'
                  );
                }}
              >
                Modify
              </Button>
            </Analytics>
          </div>
        ) : (
          <div className="flex items-center space-x-xs -my-2">
            <div onClick={() => setExpanded(!isExpanded)} className="mr-5">
              <span className="text-sm text-gray-500">More info </span>
              <FaChevronDown
                className={clsx(
                  isExpanded ? 'rotate-180' : '',
                  'transition-all duration-300 ease-in-out w-3 h-3'
                )}
              />
            </div>
            <Analytics
              name="action-tab-btn-import-openapi-create-action"
              passHtmlAttributesToChildren
            >
              <Button disabled={disabled} size="sm" onClick={onCreate}>
                Create
              </Button>
            </Analytics>
          </div>
        )}
      </div>
      <div
        className={clsx(
          'max-w-[28vw] whitespace-normal break-all',
          isExpanded ? 'h-auto pt-4' : 'h-0 pt-0',
          'overflow-hidden transition-all duration-300 ease-in-out'
        )}
      >
        {operation.description.trim() ??
          'No description available for this endpoint'}
      </div>
    </div>
  );
};
