import * as React from 'react';
import { SchemasList } from './SchemasList';
import { FeatureRequest } from './FeatureRequest';
import globals from '../../../Globals';
import { SCHEMA_REGISTRY_FEATURE_NAME } from '../constants';
import { Badge } from '../../../new-components/Badge';
import { SCHEMA_REGISTRY_REF_URL } from '../constants';

const Header: React.VFC = () => {
  return (
    <div className="flex flex-col w-full">
      <div className="flex w-full mb-sm">
        <h1 className="text-xl font-semibold">GraphQL Schema Registry</h1>
        <Badge className="mx-2" color="blue">
          BETA
        </Badge>
      </div>
      <a
        className="text-muted w-auto"
        href={SCHEMA_REGISTRY_REF_URL}
        target="_blank"
        rel="noreferrer noopener"
      >
        What is Schema Registry?
      </a>
    </div>
  );
};

const Body: React.VFC<{ hasFeatureAccess: boolean }> = props => {
  const { hasFeatureAccess } = props;

  if (!hasFeatureAccess) {
    return <FeatureRequest />;
  }

  return (
    <div className="flex w-full">
      <SchemasList />
    </div>
  );
};

export const SchemaRegistryContainer: React.VFC = () => {
  const hasFeatureAccess = globals.allowedLuxFeatures.includes(
    SCHEMA_REGISTRY_FEATURE_NAME
  );

  return (
    <div className="p-4 flex flex-col w-full">
      <div className="flex w-full mb-md">
        <Header />
      </div>
      <div className="flex w-full mb-md">
        <Body hasFeatureAccess={hasFeatureAccess} />
      </div>
    </div>
  );
};
