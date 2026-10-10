import { FaCheck, FaClock, FaExclamation } from 'react-icons/fa6';
import { FaTimes } from 'react-icons/fa';
import { IconButton } from '@hasura/shared/ui';

export const getEventStatusIcon = (status: string) => {
  switch (status) {
    case 'scheduled':
      return <FaClock title="This event has been scheduled" />;
    case 'dead':
      return (
        <FaExclamation title="This event is dead and will never be delivered" />
      );
    case 'delivered':
      return (
        <FaCheck
          className={`text-[green] text-xl`}
          aria-hidden="true"
          title="This event has been delivered"
        />
      );
    case 'error':
      return (
        <FaTimes
          className={`text-[#d9534f] text-xl`}
          aria-hidden="true"
          title="This event failed with an error"
        />
      );
    default:
      return null;
  }
};

export const getEventDeliveryIcon = (delivered: boolean) => {
  return delivered ? (
    <FaCheck title="This event has been delivered" />
  ) : (
    <FaTimes title="This event has not been delivered" />
  );
};

export const getInvocationLogStatus = (status: number) => {
  return status < 300 ? (
    <IconButton mode="success" variant="ghost" className="cursor-none">
      <FaCheck className="w-5 h-5" />
    </IconButton>
  ) : (
    <IconButton mode="destructive" variant="ghost" className="cursor-none">
      <FaTimes className="w-5 h-5" />
    </IconButton>
  );
};
