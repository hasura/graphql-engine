import { FaCheck } from 'react-icons/fa';

const Check = ({ className = '', title = '' }) => {
  return (
    <FaCheck
      className={`text-[green] text-xl ${className}`}
      aria-hidden="true"
      title={title}
    />
  );
};

export default Check;
