import { Button } from '@hasura/shared/ui';
import { Analytics } from '@hasura/shared/analytics';
import clsx from 'clsx';
import React from 'react';
import { FaChevronDown, FaExternalLinkAlt } from 'react-icons/fa';
import { Operation } from './types';
import { normalizeOperationId } from './utils';
import { Flex } from '@radix-ui/themes';
import { useAppContext } from '@hasura/shared/context';

export interface OasGeneratorActionsProps {
  operation: Operation;
  existing?: boolean;
  onCreate: () => void;
  onDelete: () => void;
  disabled?: boolean;
}

export const OasGeneratorActions: React.FC<OasGeneratorActionsProps> = (
  props,
) => {
  const { envVars } = useAppContext();
  const { operation, existing, onCreate, onDelete, disabled } = props;
  const [isExpanded, setExpanded] = React.useState(false);

  return (
    <div data-testid={`operation-${operation.operationId}`}>
      <Flex justify="between" className="cursor-pointer">
        <div className="max-w-[17vw] overflow-hidden truncate">
          {operation.path}
        </div>
        {existing ? (
          <Flex align="center" className="space-x-xs -my-2">
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
                rightIcon={FaExternalLinkAlt}
                disabled={disabled}
                size="sm"
                onClick={(e) => {
                  window.open(
                    `${envVars.urlPrefix}/actions/manage/${normalizeOperationId(
                      operation.operationId,
                    )}/modify`,
                    '_blank',
                  );
                }}
              >
                Modify
              </Button>
            </Analytics>
          </Flex>
        ) : (
          <Flex align="center" className="space-x-xs -my-2">
            <div onClick={() => setExpanded(!isExpanded)} className="mr-5">
              <span className="text-sm text-gray-500">More info </span>
              <FaChevronDown
                className={clsx(
                  isExpanded ? 'rotate-180' : '',
                  'transition-all duration-300 ease-in-out w-3 h-3',
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
          </Flex>
        )}
      </Flex>
      <div
        className={clsx(
          'max-w-[28vw] whitespace-normal break-all',
          isExpanded ? 'h-auto pt-4' : 'h-0 pt-0',
          'overflow-hidden transition-all duration-300 ease-in-out',
        )}
      >
        {operation.description.trim() ??
          'No description available for this endpoint'}
      </div>
    </div>
  );
};
