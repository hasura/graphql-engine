import { SlOptionsVertical } from 'react-icons/sl';
import {
  Button,
  CardedTable,
  Checkbox,
  DropdownMenu,
  IconButton,
  IndicatorCard,
  LearnMoreLink,
  SkeletonList,
  hasuraToast,
  showErrorNotification,
} from '@hasura/shared/ui';
import {
  useInvalidateMetadata,
  useUntrackFunctions,
} from '@hasura/metadata/api';
import { FunctionDisplayName } from './FunctionDisplayName';
import React, { useState } from 'react';
import { TrackableListMenu } from '../../components/TrackableListMenu';
import { usePaginatedSearchableList } from '../../hooks';
import { ModifyFunctionConfiguration } from '../../../ManageFunction/components/ModifyFunctionConfiguration';
import { FaEdit } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import { Source } from '@hasura/shared/types';
import { dataRoutes } from '@hasura/shared/utils';
import { TrackedFunction } from '@hasura/metadata/data-source';

export type TrackedFunctionsProps = {
  source: Source;
  trackedFunctions: TrackedFunction[];
  isLoading?: boolean;
};

export const TrackedFunctions = (props: TrackedFunctionsProps) => {
  const { source, trackedFunctions, isLoading } = props;

  const [isConfigurationModalOpen, setIsConfigurationModalOpen] =
    useState(false);

  const [activeRow, setActiveRow] = useState<number | undefined>();

  const functionsWithId = React.useMemo(() => {
    if (Array.isArray(trackedFunctions)) {
      return trackedFunctions.map((f) => ({ ...f, id: f.name }));
    } else {
      return [];
    }
  }, [trackedFunctions]);

  const invalidateMetadata = useInvalidateMetadata();
  const { untrackFunctions, isPending: isUntrackingInProgress } =
    useUntrackFunctions();

  const searchFn = React.useCallback((query, item) => {
    return item.name.toLowerCase().includes(query.toLowerCase());
  }, []);

  const listProps = usePaginatedSearchableList({
    data: functionsWithId,
    filterFn: searchFn,
  });

  const handleUntrackButton = () => {
    untrackFunctions(
      getCheckedItems().map((fn) => ({
        function: fn.function,
        source: source.name,
      })),
      {
        onSuccess: () => {
          hasuraToast({
            type: 'success',
            title: 'Success',
            message: `Untracked ${checkedIds.length} objects`,
          });
          reset();
        },
        onError: (err) => {
          showErrorNotification({
            title: 'Untracked table failed',
            error: err,
          });
        },
      },
    );
  };

  const {
    checkData: { onCheck, checkedIds, reset, checkAllElement },
    paginatedData,
    getCheckedItems,
  } = listProps;

  if (isLoading) return <SkeletonList count={5} />;

  if (!trackedFunctions.length)
    return (
      <IndicatorCard status="info" headline="No untracked functions found">
        We couldn&apos;t find any tracked functions in your metadata.{' '}
        <LearnMoreLink href="https://hasura.io/docs/latest/schema/postgres/postgres-guides/functions/" />
      </IndicatorCard>
    );
  return (
    <div className="space-y-4">
      <TrackableListMenu
        checkActionText={`Untrack Selected (${checkedIds.length})`}
        handleTrackButton={handleUntrackButton}
        isLoading={isUntrackingInProgress}
        {...listProps}
        showButton
      />
      <CardedTable
        columns={[
          checkAllElement(),
          'Function',
          <Flex key="options-column" justify="end" align="center">
            <DropdownMenu.Root
              items={[
                <DropdownMenu.Item
                  key="refresh"
                  onSelect={() =>
                    invalidateMetadata({
                      componentName: 'TrackedFunctions',
                      reasons: [
                        'Refreshing tracked functions on dropdown menu item click.',
                      ],
                    })
                  }
                >
                  Refresh
                </DropdownMenu.Item>,
              ]}
              options={{
                content: {
                  alignOffset: -50,
                  avoidCollisions: false,
                },
              }}
            >
              <IconButton variant="ghost" radius="full">
                <SlOptionsVertical />
              </IconButton>
            </DropdownMenu.Root>
          </Flex>,
        ]}
        data={paginatedData.map((trackedFunction, index) => [
          <Checkbox
            key={`check-${trackedFunction.name}`}
            value={checkedIds.includes(trackedFunction.name)}
            onChange={() => onCheck(trackedFunction.name)}
          />,
          <FunctionDisplayName
            key={`name-${trackedFunction.name}`}
            qualifiedFunction={trackedFunction.function}
            to={dataRoutes.manageFunction(
              source.name,
              trackedFunction.function,
            )}
          />,
          <Flex key={`actions-${trackedFunction.name}`} gap="2" justify="end">
            {isConfigurationModalOpen ? (
              <ModifyFunctionConfiguration
                source={source}
                currentFunction={trackedFunction}
                onSuccess={() => setIsConfigurationModalOpen(false)}
                onClose={() => setIsConfigurationModalOpen(false)}
              />
            ) : null}
            <Button
              mode="default"
              onClick={() => {
                setIsConfigurationModalOpen(true);
              }}
              leftIcon={FaEdit}
            >
              Configure
            </Button>
            <Button
              mode="destructive"
              onClick={() => {
                setActiveRow(index);
                untrackFunctions(
                  [
                    {
                      function: trackedFunction.function,
                      source: source.name,
                    },
                  ],
                  {
                    onSuccess: () => {
                      hasuraToast({
                        type: 'success',
                        title: 'Success',
                        message: `Untracked object`,
                      });
                      setActiveRow(undefined);
                    },
                    onError: (error) => {
                      showErrorNotification({
                        title: 'Untracking failed',
                        error,
                      });
                    },
                  },
                );
              }}
              loading={activeRow === index && isUntrackingInProgress}
              loadingText="Please wait"
            >
              Untrack
            </Button>
          </Flex>,
        ])}
      />
    </div>
  );
};
