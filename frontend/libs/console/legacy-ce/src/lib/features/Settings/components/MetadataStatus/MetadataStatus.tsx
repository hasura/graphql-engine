import { useMemo, useState } from 'react';
import { createColumnHelper } from '@tanstack/react-table';
import {
  Button,
  DataTable,
  DataTableFeatures,
  IndicatorCard,
  Text,
} from '@hasura/shared/ui';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { getConfirmation } from '@hasura/shared/utils';
import { useNavigate } from 'react-router';
import ReloadMetadata from '../MetadataOptions/ReloadMetadata';
import {
  useInconsistentMetadata,
  useMetadata,
  useDropInconsistentMetadata,
} from '@hasura/metadata/api';

import {
  getInconsistentObjectDisplay,
  InconsistentObjectActionCell,
  InconsistentObjectReasonCell,
  resolveInconsistentInheritedRole,
} from './InconsistentObjectRow';
import {
  InconsistentObject,
  InconsistentObjectFields,
  InconsistentObjectType,
} from '@hasura/shared/types';
import { Flex, Heading, Strong } from '@radix-ui/themes';

const PAGE_SIZE = 10;

const flattenInconsistentObjects = (
  inconsistentObjects: InconsistentObject[] | undefined,
): InconsistentObjectFields[] => {
  if (!inconsistentObjects?.length) {
    return [];
  }

  return inconsistentObjects.flatMap((inconsistentObject) => {
    if ('type' in inconsistentObject) {
      return [inconsistentObject];
    }

    const objects =
      'objects' in inconsistentObject
        ? inconsistentObject.objects
        : 'conflicts' in inconsistentObject
          ? inconsistentObject.conflicts
          : inconsistentObject.definitions;

    return objects.map((obj): InconsistentObjectFields => ({
      ...(obj as any),
      type:
        obj.type ||
        (('type' in inconsistentObject
          ? inconsistentObject.type
          : '') as InconsistentObjectType),
      reason: obj.reason || inconsistentObject.reason,
    }));
  });
};

const columnHelper = createColumnHelper<
  DataTableFeatures,
  InconsistentObjectFields
>();

const MetadataStatus = () => {
  const navigate = useNavigate();
  const dropInconsistentMetadata = useDropInconsistentMetadata();
  const { data: inconsistentMetadata, refetch: refreshInconsistentMetadata } =
    useInconsistentMetadata();
  const { data: metadataData, refetch: refreshMetadata } = useMetadata();
  const [shouldShowErrorBanner, toggleErrorBanner] = useState(true);
  const [isLoading, setIsLoading] = useState(false);
  const dismissErrorBanner = () => {
    toggleErrorBanner(false);
  };

  const isConsistentMetadata =
    !inconsistentMetadata || inconsistentMetadata.is_consistent;

  const inconsistentObjects = useMemo(
    () =>
      flattenInconsistentObjects(inconsistentMetadata?.inconsistent_objects),
    [inconsistentMetadata?.inconsistent_objects],
  );

  const columns = useMemo(
    () =>
      columnHelper.columns([
        columnHelper.display({
          id: 'action',
          header: 'Action',
          cell: ({ row }) => (
            <InconsistentObjectActionCell
              inconsistentObject={row.original}
              onResolve={(inconsistentInheritedRoleObj) =>
                resolveInconsistentInheritedRole(
                  navigate,
                  metadataData?.metadata,
                  inconsistentInheritedRoleObj,
                )
              }
            />
          ),
        }),
        columnHelper.display({
          id: 'name',
          header: 'Name',
          cell: ({ row }) => {
            const { name } = getInconsistentObjectDisplay(row.original);
            return (
              <Text>
                <Strong>{row.original.type}</Strong>
                <br />
                {name}
              </Text>
            );
          },
        }),
        columnHelper.display({
          id: 'description',
          header: 'Description',
          cell: ({ row }) => {
            const { definition } = getInconsistentObjectDisplay(row.original);
            return definition;
          },
        }),
        columnHelper.display({
          id: 'reason',
          header: 'Reason',
          cell: ({ row }) => (
            <InconsistentObjectReasonCell inconsistentObject={row.original} />
          ),
        }),
      ]),
    [navigate, metadataData?.metadata],
  );

  const verifyAndDropAll = () => {
    const confirmMessage = `This will drop all the inconsistent objects in your metadata. This includes all inconsistent sources - databases, remote schemas, actions etc. and any Hasura features related to these objects. This action is irreversible.`;
    const isOk = getConfirmation(confirmMessage);
    if (isOk) {
      setIsLoading(true);
      return dropInconsistentMetadata()
        .then(() => {
          refreshMetadata();
          refreshInconsistentMetadata();
        })
        .finally(() => {
          setIsLoading(false);
        });
    }
  };

  const content = () => {
    const isInconsistentRemoteSchemaPresent =
      inconsistentMetadata?.inconsistent_objects?.some(
        (i) => 'type' in i && i.type === 'remote_schema',
      ) ?? false;
    if (isConsistentMetadata) {
      return (
        <div className="mt-4">
          <IndicatorCard status="positive" showIcon>
            GraphQL Engine metadata is consistent with database
          </IndicatorCard>
        </div>
      );
    }

    return (
      <div>
        <div className="my-4">
          <IndicatorCard
            status="negative"
            showIcon
            headline="GraphQL Engine metadata is inconsistent with database"
          >
            <Text as="p">
              The following objects in your metadata are inconsistent because
              they reference database or remote-schema entities which do not
              seem to exist or are conflicting
            </Text>
            <Text as="p">
              The GraphQL API has been generated using only the consistent parts
              of the metadata
            </Text>
            <Text as="p">
              The console might also not be able to display these inconsistent
              objects
            </Text>
          </IndicatorCard>
        </div>
        <DataTable
          columns={columns}
          data={inconsistentObjects}
          pageSize={PAGE_SIZE}
          dataTestId="inconsistent-objects-table"
          noRowsMessage="No inconsistent objects found."
        />
        <div className="my-4">
          <Text as="p">
            To resolve these inconsistencies, you can do one of the following:
          </Text>
          <ul className="mt-2 list-disc pl-4">
            <li>
              <Text>
                To delete all the inconsistent objects from the metadata, click
                the &quot;Delete all&quot; button
              </Text>
            </li>
            <li>
              <Text>
                If you want to manage these objects on your own, please do so
                and click on the &quot;Reload Metadata&quot; button to check if
                the inconsistencies have been resolved
              </Text>
            </li>
          </ul>
        </div>
        <Flex gap="4">
          <Button
            mode="destructive"
            onClick={verifyAndDropAll}
            loading={isLoading}
            loadingText={'Deleting...'}
          >
            Delete all
          </Button>
          <ReloadMetadata
            buttonText="Reload metadata"
            showReloadRemoteSchemas={isInconsistentRemoteSchemaPresent}
          />
        </Flex>
      </div>
    );
  };

  const banner = () => {
    if (isConsistentMetadata) {
      return null;
    }

    if (!shouldShowErrorBanner) {
      return null;
    }
    const urlSearchParams = new URLSearchParams(window.location.search);
    if (
      urlSearchParams.get('is_redirected') !== 'true' &&
      !isConsistentMetadata
    ) {
      return null;
    }
    return (
      <div
        className={`w-full p-4 bg-red-100 items-center flex justify-between flex-row border border-red-200`}
      >
        <IndicatorCard
          showIcon
          status="negative"
          onDismiss={dismissErrorBanner}
        >
          You have been redirected because your GraphQL Engine metadata is in an
          inconsistent state
        </IndicatorCard>
      </div>
    );
  };

  return (
    <Analytics name="MetadataStatus" {...REDACT_EVERYTHING}>
      <div className="p-4">
        {banner()}
        <Flex direction="column" gap="4">
          <Heading size="6">Hasura Metadata Status</Heading>
          {content()}
        </Flex>
      </div>
    </Analytics>
  );
};

export default MetadataStatus;
