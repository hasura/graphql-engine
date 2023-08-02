import { useDispatch } from 'react-redux';
import _push from '../../../components/Services/Data/push';

export const usePushRoute = () => {
  const dispatch = useDispatch();

  const pushRoute = (path: string) => {
    dispatch(_push(path));
  };

  return pushRoute;
};
