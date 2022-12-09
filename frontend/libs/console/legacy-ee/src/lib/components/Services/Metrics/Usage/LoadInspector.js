import React from 'react';

import Modal from '../Common/UsageInspectModal';

const LoadInspector = props => {
  const { RenderLink, data, onHide, groupBys } = props;
  return (
    <Modal
      RenderLink={RenderLink}
      data={data}
      groupBys={groupBys}
      onHide={onHide}
    />
  );
};

export default LoadInspector;
