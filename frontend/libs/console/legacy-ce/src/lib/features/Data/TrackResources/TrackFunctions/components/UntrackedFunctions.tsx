import { SlOptionsVertical } from 'react-icons/sl';
import { Flex } from '@radix-ui/themes';
import {
  Button,
  CardedTable,
  DropdownMenu,
  IndicatorCard,
  LearnMoreLink,
  useHasuraAlert,
  hasuraToast,
  SkeletonList,
  showErrorNotification,
} from '@hasura/shared/ui';
import {
  IntrospectedFunction,
  useInvalidateTrackableFunctions,
} from '@hasura/metadata/data-source';
import {
  useInvalidateMetadata,
  useMetadata,
  useTrackFunctions,
} from '@hasura/metadata/api';
import { FunctionDisplayName } from './FunctionDisplayName';
import React, { useState } from 'react';
import { TableFunction } from '@hasura/shared/types';
import { TrackableListMenu } from '../../components/TrackableListMenu';
import { usePaginatedSearchableList } from '../../hooks';
import {
  TrackFunctionForm,
  TrackFunctionFormSchema,
} from './TrackFunctionForm';
import {
  isNativeDriver as _isNativeDriver,
  MetadataSelectors,
} from '@hasura/metadata/helpers';

export type UntrackedFunctionsProps = {
  dataSourceName: string;
  isLoading?: boolean;
  untrackedFunctions: IntrospectedFunction[];
};

export type AllowedFunctionTypes = 'mutation' | 'query' | 'root_field';

export const UntrackedFunctions = (props: UntrackedFunctionsProps) => {
  const { dataSourceName, untrackedFunctions = [], isLoading } = props;

  const invalidateUntrackedFunctions = useInvalidateTrackableFunctions();
  const invalidateMetadata = useInvalidateMetadata();

  const [activeRow, setActiveRow] = useState<number | undefined>();

  const [isModalOpen, setIsModalOpen] = useState(false);
  const [modalFormDefaultValues, setModalFormDefaultValues] =
    useState<TrackFunctionFormSchema>();

  const [activeOperation, setActiveOperation] =
    useState<AllowedFunctionTypes>();

  const { hasuraConfirm } = useHasuraAlert();

  const { data: driver = '' } = useMetadata(
    (m) => MetadataSelectors.findSource(dataSourceName)(m)?.kind,
  );

  const isNativeDriver = _isNativeDriver(driver);

  const { trackFunctions, isPending: isTrackingInProgress } =
    useTrackFunctions();

  const functionsWithId = Array.isArray(untrackedFunctions)
    ? untrackedFunctions.map((f) => ({ ...f, id: f.name }))
    : [];

  const listProps = usePaginatedSearchableList({
    data: functionsWithId,
    filterFn: (query, item) => {
      return item.name.toLowerCase().includes(query.toLowerCase());
    },
  });

  if (isLoading) return <SkeletonList count={5} />;

  if (!untrackedFunctions.length)
    return (
      <IndicatorCard status="info" headline="No untracked functions found">
        We couldn&apos;t find any compatible functions in your database that can
        be tracked in Hasura.{' '}
        <LearnMoreLink href="https://hasura.io/docs/latest/schema/postgres/postgres-guides/functions/" />
      </IndicatorCard>
    );

  const {
    checkData: { checkedIds },
    paginatedData,
  } = listProps;

  const handleTrack = (
    index: number,
    fn: TableFunction,
    type: AllowedFunctionTypes,
  ) => {
    // hack until capabilities or function schema can tell us if the function supports return types
    if (!isNativeDriver) {
      setModalFormDefaultValues({
        qualifiedFunction: JSON.stringify(fn),
        type,
        table: '',
      });
      setIsModalOpen(true);
      return;
    }

    setActiveRow(index);
    setActiveOperation(type);
    trackFunctions(
      [
        {
          source: dataSourceName,
          function: fn,
          ...(type !== 'root_field'
            ? {
                configuration: {
                  exposed_as: type,
                },
              }
            : {}),
        },
      ],
      {
        onSuccess: () => {
          hasuraToast({
            type: 'success',
            title: 'Success',
            message: `Tracked object successfully`,
          });
        },
        onError: (err) => {
          showErrorNotification({
            title: 'Tracking function failed',
            error: err,
          });
        },
        onSettled: () => {
          setActiveRow(undefined);
          setActiveOperation(undefined);
        },
      },
    );
  };

  return (
    <div>
      <div className="space-y-4">
        <TrackableListMenu
          checkActionText={`Track Selected (${checkedIds.length})`}
          isLoading={isLoading ?? false}
          {...listProps}
        />
        <CardedTable
          columns={[
            'Function',
            <Flex key="options-column" align="center" justify="end">
              <DropdownMenu.Root
                items={[
                  <DropdownMenu.Item
                    key="refresh"
                    onSelect={() => {
                      invalidateUntrackedFunctions(dataSourceName);
                      invalidateMetadata({
                        componentName: 'UntrackedFunctions',
                        reasons: [
                          'Refreshing untracked functions on Dropdown Menu item click.',
                        ],
                      });
                    }}
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
                <SlOptionsVertical />
              </DropdownMenu.Root>
            </Flex>,
          ]}
          data={paginatedData.map((untrackedFunction, index) => [
            <FunctionDisplayName
              key={`name-${untrackedFunction.name}`}
              qualifiedFunction={untrackedFunction.function}
            />,
            <Flex
              key={`actions-${untrackedFunction.name}`}
              gap="2"
              justify="end"
            >
              {untrackedFunction.isVolatile ? (
                <>
                  <Button
                    mode="default"
                    onClick={() => {
                      handleTrack(
                        index,
                        untrackedFunction.function,
                        'mutation',
                      );
                    }}
                    loading={
                      activeRow === index &&
                      isTrackingInProgress &&
                      activeOperation === 'mutation'
                    }
                    disabled={activeRow === index && isTrackingInProgress}
                    loadingText="Please wait..."
                  >
                    Track as Mutation
                  </Button>
                  <Button
                    mode="primary"
                    onClick={() =>
                      hasuraConfirm({
                        message:
                          'Queries are supposed to be read only and as such recommended to be STABLE or IMMUTABLE',
                        title: `Confirm tracking ${untrackedFunction.name} as a query`,
                        onClose: ({ confirmed }) => {
                          if (confirmed)
                            handleTrack(
                              index,
                              untrackedFunction.function,
                              'query',
                            );
                        },
                      })
                    }
                    loading={
                      activeRow === index &&
                      isTrackingInProgress &&
                      activeOperation === 'query'
                    }
                    disabled={activeRow === index && isTrackingInProgress}
                    loadingText="Please wait..."
                  >
                    Track as Query
                  </Button>
                </>
              ) : (
                <Button
                  mode="default"
                  size="1"
                  onClick={() => {
                    handleTrack(
                      index,
                      untrackedFunction.function,
                      'root_field',
                    );
                  }}
                  loading={
                    activeRow === index &&
                    isTrackingInProgress &&
                    activeOperation === 'root_field'
                  }
                  disabled={activeRow === index && isTrackingInProgress}
                  loadingText="Please wait..."
                >
                  Track as Root Field
                </Button>
              )}
            </Flex>,
          ])}
        />
      </div>
      <div>
        {isModalOpen ? (
          <TrackFunctionForm
            dataSourceName={dataSourceName}
            onSuccess={() => setIsModalOpen(false)}
            onClose={() => setIsModalOpen(false)}
            defaultValues={modalFormDefaultValues}
          />
        ) : null}
      </div>
    </div>
  );
};
