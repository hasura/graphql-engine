import React from 'react';

import WarningSymbol from '../WarningSymbol/WarningSymbol';

const gqlPattern = /^[_A-Za-z][_0-9A-Za-z]*$/;

export interface GqlCompatibilityWarningProps {
  identifier: string;
  className?: string;
}

const GqlCompatibilityWarning: React.FC<GqlCompatibilityWarningProps> = ({ identifier, className }) => {
  const isGraphQLCompatible = gqlPattern.test(identifier);

  if (isGraphQLCompatible) {
    return null;
  }

  const gqlCompatibilityTip =
    'This identifier name does not conform to the GraphQL naming standard. ' +
    'Names in GraphQL should be limited to this ASCII subset: /[_A-Za-z][_0-9A-Za-z]*/. ' +
    'All GraphQL types depending on this identifier will not be exposed over the GraphQL API';

  return (
    <span className={className}>
      <WarningSymbol tooltipText={gqlCompatibilityTip} />
    </span>
  );
};

export default GqlCompatibilityWarning;
