import { Card, IndicatorCard, Tabs } from '@hasura/shared/ui';
import {
  AccessType,
  DataQueryType,
  MetadataTable,
  Source,
} from '@hasura/shared/types';
import { Skeleton } from '@radix-ui/themes';
import PermissionsFormWrapper from './components/ModifyPermission';
import { ClonePermissionsSection } from './components';
import { useMetadata } from '@hasura/metadata/api';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import { useRef } from 'react';
import { useScrollIntoView } from '@hasura/shared/hooks';

export interface PermissionsFormProps {
  source: Source;
  table: MetadataTable;
  queryType: DataQueryType;
  roleName: string;
  accessType: AccessType;
  handleClose: () => void;
}

// necessary to wrap in this component as otherwise default values are not set properly in useConsoleForm
export const PermissionsForm = (props: PermissionsFormProps) => {
  const { data: meta, isFetching: metadataFetching } = useMetadata();

  const permissionSectionRef = useRef(null);
  useScrollIntoView(permissionSectionRef, [], { behavior: 'smooth' });

  if (!meta) {
    if (metadataFetching) {
      return <Skeleton width="100%" height="300px" />;
    }

    return (
      <IndicatorCard status="negative" showIcon>
        Failed to load metadata. Please try again later.
      </IndicatorCard>
    );
  }

  const currentPermission = MetadataSelectors.findPermissionByQueryAndRole(
    props.table,
    props.queryType,
    props.roleName,
  );

  // Issue #10907: metadata is a single shared react-query cache. On client-side
  // (SPA) navigation the form can mount while that cache is still refetching, so
  // `props.table` may not yet carry this role's permission. The form seeds
  // react-hook-form from metadata-derived default values exactly once on mount
  // (PermissionsFormComponent's `useConsoleForm`), so mounting against the
  // incomplete snapshot leaves the Row radio and Column checkboxes blank until a
  // full page refresh. When the permissions table says a permission exists
  // (`accessType !== 'noAccess'`) but we can't find it yet and metadata is still
  // fetching, wait for hydration before mounting the form.
  const awaitingPermissionHydration =
    props.accessType !== 'noAccess' && !currentPermission && metadataFetching;

  if (awaitingPermissionHydration) {
    return <Skeleton width="100%" height="300px" />;
  }

  // Remount the form on any role/table/query/source change. The permissions
  // table machine keeps the form open across these switches, and react-hook-form
  // only applies default values on mount, so without a stable identity key the
  // form would keep the previously selected permission's values. A value-based
  // key is deliberately avoided so a background metadata refetch does not
  // remount the form and discard the user's in-progress edits.
  const formKey = `${props.source.name}-${JSON.stringify(props.table.table)}-${props.roleName}-${props.queryType}`;

  const renderContent = () => {
    if (props.accessType === 'noAccess' || !currentPermission) {
      return (
        <PermissionsFormWrapper
          key={formKey}
          {...props}
          metadata={meta.metadata}
          showCloseButton
        />
      );
    }

    return (
      <Tabs
        items={[
          {
            label: 'Modify',
            value: 'modify',
            content: (
              <PermissionsFormWrapper
                key={formKey}
                {...props}
                metadata={meta.metadata}
              />
            ),
          },
          {
            label: 'Clone Permissions',
            value: 'clone',
            content: (
              <ClonePermissionsSection
                queryType={props.queryType}
                source={props.source}
                metadata={meta.metadata}
                table={props.table.table}
                onClose={props.handleClose}
                permission={currentPermission}
              />
            ),
          },
        ]}
      />
    );
  };

  return (
    <Card size="3" data-testid="permissions-form">
      {renderContent()}
      <div ref={permissionSectionRef}></div>
    </Card>
  );
};
