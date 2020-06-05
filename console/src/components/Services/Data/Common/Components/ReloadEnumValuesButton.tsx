import React from 'react';
import { ThunkDispatch } from 'redux-thunk';
import { AnyAction } from 'redux';
import ReloadMetadata from '../../../Settings/MetadataOptions/ReloadMetadata';

export interface ReloadEnumValuesButtonProps {
  isEnum: boolean;
  dispatch: ThunkDispatch<{}, {}, AnyAction>;
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
      buttonToolTip={TOOLTIP_TEXT}
      checkBoxVisible={false}
      dispatch={dispatch}
      tooltipStyle={tooltipStyle}
    />
  );
};

export default ReloadEnumValuesButton;
