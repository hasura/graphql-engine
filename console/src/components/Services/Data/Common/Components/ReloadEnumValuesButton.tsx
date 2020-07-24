import React from 'react';
import { ThunkDispatch } from 'redux-thunk';
import { AnyAction } from 'redux';
import ReloadMetadata from '../../../Settings/MetadataOptions/ReloadMetadata';

export interface ReloadEnumValuesButtonProps {
  dispatch: ThunkDispatch<{}, {}, AnyAction>;
  tooltipStyle?: string;
}

const TOOLTIP_TEXT =
  'Reload enum values in your GraphQL schema after inserting, updating or deleting enum values';

const ReloadEnumValuesButton: React.FC<ReloadEnumValuesButtonProps> = ({
  dispatch,
  tooltipStyle = '',
}) => (
  <ReloadMetadata
    buttonText="Reload enum values"
    btnTooltipMessage={TOOLTIP_TEXT}
    showReloadRemoteSchemas={false}
    dispatch={dispatch}
    tooltipStyle={tooltipStyle}
  />
);

export default ReloadEnumValuesButton;
