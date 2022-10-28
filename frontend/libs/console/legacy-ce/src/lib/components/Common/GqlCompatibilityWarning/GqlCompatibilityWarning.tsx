import React from 'react';

import WarningSymbol from '../WarningSymbol/WarningSymbol';

const gqlPattern = /^[_A-Za-z][_0-9A-Za-z]*$/;

export interface GqlCompatibilityWarningProps {
  identifier: string;
  className?: string;
  ifWarningCanBeFixed?: boolean;
}

const GqlCompatibilityWarning: React.FC<GqlCompatibilityWarningProps> = ({
  identifier,
  className = '',
  ifWarningCanBeFixed = false,
}) => {
  const isGraphQLCompatible = gqlPattern.test(identifier);

  if (isGraphQLCompatible) {
    return null;
  }

  const gqlCompatibilityTip =
    'This identifier name does not conform to the GraphQL naming standard. ' +
    'Names in GraphQL should be limited to this ASCII subset: /[_A-Za-z][_0-9A-Za-z]*/. ' +
    'All GraphQL types depending on this identifier will not be exposed over the GraphQL API';

  const gqlCompatibilityTipWithFix =
    'This identifier name does not conform to the GraphQL naming standard. ' +
    'Names in GraphQL should be limited to this ASCII subset: /[_A-Za-z][_0-9A-Za-z]*/. ' +
    'Invalid characters in this identifier will be replaced with underscores. ' +
    'If a table or column name starts with number(s), ' +
    'the number(s) will be replaced by "table_" or "column_" respectively';

  return (
    <span className={className}>
      <WarningSymbol
        tooltipText={
          !ifWarningCanBeFixed
            ? gqlCompatibilityTip
            : gqlCompatibilityTipWithFix
        }
      />
    </span>
  );
};

export default GqlCompatibilityWarning;
