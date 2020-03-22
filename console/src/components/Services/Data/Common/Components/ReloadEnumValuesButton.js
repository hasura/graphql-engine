import React from 'react';
import ReloadEnumMetadata from '../../../Settings/MetadataOptions/ReloadMetadata';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Tooltip from 'react-bootstrap/lib/Tooltip';

import { Icon } from '../../../../UIKit/atoms';
import styles from '../../../../Common/Common.scss';

const ReloadEnumValuesButton = ({ isEnum, dispatch, tooltipStyle }) => {
  if (!isEnum) return null;

  const tooltip = (
    <Tooltip id="tooltip-reload-enum-metadata">
      Reload enum values in your GraphQL schema after inserting, updating or
      deleting enum values
    </Tooltip>
  );

  return (
    <React.Fragment>
      <ReloadEnumMetadata buttonText="Reload enum values" dispatch={dispatch} />
      <OverlayTrigger overlay={tooltip} placement="right">
        <Icon
          type="info"
          className={`${styles.cursorPointer} ${tooltipStyle || ''}`}
        />
      </OverlayTrigger>
    </React.Fragment>
  );
};

export default ReloadEnumValuesButton;
