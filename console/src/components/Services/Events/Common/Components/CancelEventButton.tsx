import React from 'react';
import Button from '../../../../Common/Button';

type CancelButtonProps = {
  id: string;
  handler: () => void;
};

const CancelEventButton: React.FC<CancelButtonProps> = ({ id, handler }) => {
  const onClickHandler = () => handler();

  return (
    <Button
      key={id}
      onClick={onClickHandler}
      color="white"
      size="xs"
      title="Cancel Event"
    >
      <i className="fa fa-close" />
    </Button>
  );
};

export default CancelEventButton;
