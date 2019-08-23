import React from 'react';

import WarningSymbol from '../WarningSymbol/WarningSymbol';

const GqlCompatibilityWarning = () => {
  const gqlCompatibilityTip =
    'This identifier name does not conform to the GraphQL naming standard. Names in GraphQL should be limited to this ASCII subset: /[_A-Za-z][_0-9A-Za-z]*/.';

  return <WarningSymbol tooltipText={gqlCompatibilityTip} />;
};

export default GqlCompatibilityWarning;
