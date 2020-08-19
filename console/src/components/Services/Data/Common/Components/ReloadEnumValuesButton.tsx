import React from 'react';
import ReloadMetadata from '../../../Settings/MetadataOptions/ReloadMetadata';
import { Dispatch } from '../../../../../types';

export interface ReloadEnumValuesButtonProps {
  isEnum: boolean;
  dispatch: Dispatch;
  tooltipStyle?: string;
}

const TOOLTIP_TEXT =
  'Reload enum values in your GraphQL schema after inserting, updating or deleting enum values';

const ReloadEnumValuesButton: React.FC<ReloadEnumValuesButtonProps> = ({
  isEnum,
  dispatch,
  tooltipStyle = '',
}) => {
  if (!isEnum) return null;
  return (
    <ReloadMetadata
      buttonText="Reload enum values"
      btnTooltipMessage={TOOLTIP_TEXT}
      showReloadRemoteSchemas={false}
      dispatch={dispatch}
      tooltipStyle={tooltipStyle}
    />
  );
};

export default ReloadEnumValuesButton;
