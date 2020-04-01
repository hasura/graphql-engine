import React from 'react';

import ReloadEnumMetadata from '../../../Settings/MetadataOptions/ReloadMetadata';
import { Icon, ToolTip } from '../../../../UIKit/atoms';

// eslint-disable-next-line @typescript-eslint/no-unused-vars
const ReloadEnumValuesButton = ({ isEnum, dispatch }) => {
  if (!isEnum) return null;

  const tooltipText =
    'Reload enum values in your GraphQL schema after inserting, updating or deleting enum values';

  return (
    <React.Fragment>
      <ReloadEnumMetadata buttonText="Reload enum values" dispatch={dispatch} />
      <ToolTip message={tooltipText}>
        <Icon type="info" pointer ml="sm" />
      </ToolTip>
    </React.Fragment>
  );
};

export default ReloadEnumValuesButton;
