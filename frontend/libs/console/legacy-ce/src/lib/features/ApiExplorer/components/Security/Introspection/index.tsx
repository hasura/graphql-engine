import React from 'react';
import { SecurityTabs } from '../SecurityTabs';
import IntrospectionTable from './IntrospectionTable';
import { useMetadata } from '@hasura/metadata/api';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import { Skeleton } from '@radix-ui/themes';

const IntrospectionOptions: React.FC = () => {
  const { data: meta, isFetching } = useMetadata();

  const disabledRoles =
    meta?.metadata?.graphql_schema_introspection?.disabled_for_roles ?? [];

  const tableData = MetadataSelectors.getRoles(meta?.metadata).map((role) => ({
    roleName: role,
    introspectionIsDisabled: disabledRoles.includes(role),
  }));

  return (
    <SecurityTabs tabName="introspection">
      <Skeleton loading={isFetching}>
        <IntrospectionTable rows={tableData} />
      </Skeleton>
    </SecurityTabs>
  );
};

export default IntrospectionOptions;
