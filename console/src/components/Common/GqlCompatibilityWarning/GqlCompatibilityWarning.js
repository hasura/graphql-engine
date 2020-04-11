import React from 'react';

import { ToolTip, Icon } from '../../UIKit/atoms';

const gqlPattern = /^[_A-Za-z][_0-9A-Za-z]*$/;

const GqlCompatibilityWarning = ({ identifier }) => {
  const isGraphQLCompatible = gqlPattern.test(identifier);

  if (isGraphQLCompatible) {
    return null;
  }

  const gqlCompatibilityTip =
    'This identifier name does not conform to the GraphQL naming standard. ' +
    'Names in GraphQL should be limited to this ASCII subset: /[_A-Za-z][_0-9A-Za-z]*/. ' +
    'All GraphQL types depending on this identifier will not be exposed over the GraphQL API';

  return (
    <ToolTip message={gqlCompatibilityTip}>
      <Icon type='warning' size={12} ml='sm' />
    </ToolTip>
  );
};

export default GqlCompatibilityWarning;
