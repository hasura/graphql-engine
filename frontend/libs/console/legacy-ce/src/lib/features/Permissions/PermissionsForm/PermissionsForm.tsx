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

  const renderContent = () => {
    if (props.accessType === 'noAccess' || !currentPermission) {
      return (
        <PermissionsFormWrapper
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
              <PermissionsFormWrapper {...props} metadata={meta.metadata} />
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
