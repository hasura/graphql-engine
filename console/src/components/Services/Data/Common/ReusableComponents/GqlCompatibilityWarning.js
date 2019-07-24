import React from 'react';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';

import styles from '../../../../Common/Common.scss';

const GqlCompatibilityWarning = () => {
  const gqlCompatibilityTip = (
    <Tooltip id="tooltip-scheme-warning">
      This identifier name does not conform to the GraphQL naming standard.
      Names in GraphQL should be limited to this ASCII subset:
      /[_A-Za-z][_0-9A-Za-z]*/.
    </Tooltip>
  );

  return (
    <div className={styles.display_inline}>
      <OverlayTrigger placement="right" overlay={gqlCompatibilityTip}>
        <i
          className={`fa fa-exclamation-triangle ${styles.add_mar_left_small}`}
          aria-hidden="true"
        />
      </OverlayTrigger>
    </div>
  );
};

export default GqlCompatibilityWarning;
