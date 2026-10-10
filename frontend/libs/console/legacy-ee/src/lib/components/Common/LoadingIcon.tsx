import { FaCircleNotch } from 'react-icons/fa';

const LoadingIcon = ({ loading = true }) =>
  loading && <FaCircleNotch className="ml-1.5" />;

export default LoadingIcon;
