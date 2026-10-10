import {
  Badge,
  DropdownButton,
  DropdownMenu,
  hasuraToast,
  showErrorNotification,
  Text,
  useDestructiveConfirm,
} from '@hasura/shared/ui';
import React from 'react';
import { QualifiedDataSource, TableFunction } from '@hasura/shared/types';
import { FunctionDisplayName } from '../../TrackResources/TrackFunctions/components/FunctionDisplayName';
import { useNavigate } from 'react-router';
import { useUntrackFunctions } from '@hasura/metadata/api';
import { Flex } from '@radix-ui/themes';
import { dataRoutes } from '@hasura/shared/utils';
import {
  getDatabaseMethods,
  useDropFunction,
} from '@hasura/metadata/data-source';
import { functionDisplayName } from '@hasura/metadata/helpers';

export const Heading: React.FC<{
  source: QualifiedDataSource;
  qualifiedFunction: TableFunction;
}> = ({ qualifiedFunction, source }) => {
  const navigate = useNavigate();
  const destructiveConfirm = useDestructiveConfirm();
  const { untrackFunctions, isPending: isUntracking } = useUntrackFunctions();
  const { mutateAsync: dropFunction, isPending: isDropping } =
    useDropFunction();

  const onUntrackClick = () => {
    untrackFunctions(
      [
        {
          function: qualifiedFunction,
          source: source.name,
        },
      ],
      {
        onSuccess: () => {
          hasuraToast({
            type: 'success',
            title: 'Successfully untracked function',
          });
          navigate(dataRoutes.manageDatabaseSource(source.name, 'functions'));
        },
        onError: (err) => {
          showErrorNotification({
            title: 'Error while untracking table',
            error: err,
          });
        },
      },
    );
  };

  const onDropFunction = () => {
    destructiveConfirm({
      resourceName: functionDisplayName({
        qualifiedFunction,
      }),
      resourceType: 'function',
      onConfirm: () =>
        dropFunction({
          func: qualifiedFunction,
          source,
        })
          .then(() => {
            navigate(dataRoutes.manageDatabaseSource(source.name, 'functions'));
            return true;
          })
          .catch(() => false),
    });
  };

  const disabled = isUntracking || isDropping;
  const dbMethods = getDatabaseMethods(source.kind);

  return (
    <Flex align="center" gap="3" className="my-4">
      <DropdownButton
        variant="ghost"
        items={[
          <DropdownMenu.Item
            key="untrack"
            onSelect={onUntrackClick}
            disabled={disabled}
          >
            Untrack
          </DropdownMenu.Item>,
        ].concat(
          dbMethods.modify?.dropFunction ? (
            <DropdownMenu.Item
              color="red"
              onSelect={onDropFunction}
              disabled={disabled}
            >
              Delete
            </DropdownMenu.Item>
          ) : (
            []
          ),
        )}
      >
        <Text weight="bold">
          <FunctionDisplayName qualifiedFunction={qualifiedFunction} />
        </Text>
      </DropdownButton>
      <div>
        <Badge color="green">Tracked</Badge>
      </div>
    </Flex>
  );
};
