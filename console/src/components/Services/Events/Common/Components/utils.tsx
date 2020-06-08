import React from 'react';
import CheckIcon from '../../../../Common/Icons/Check';
import CrossIcon from '../../../../Common/Icons/Cross';
import ClockIcon from '../../../../Common/Icons/Clock';
import Skull from '../../../../Common/Icons/Invalid';

export const getEventStatusIcon = (status: string) => {
  switch (status) {
    case 'scheduled':
      return <ClockIcon className="" title="This event has been scheduled" />;
      break;
    case 'dead':
      return (
        <Skull
          className=""
          title="This event is dead and will never be delivered"
        />
      );
      break;
    case 'delivered':
      return <CheckIcon className="" title="This event has been delivered" />;
      break;
    case 'error':
      return <CrossIcon className="" title="This event failed with an error" />;
      break;
    default:
      return null;
      break;
  }
};

export const getEventDeliveryIcon = (delivered: boolean) => {
  return delivered ? (
    <CheckIcon className="" title="This event has been delivered" />
  ) : (
    <CrossIcon className="" title="This event has not been delivered" />
  );
};

export const getInvocationLogStatus = (status: number) => {
  return status < 300 ? <CheckIcon className="" /> : <CrossIcon className="" />;
};
