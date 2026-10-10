import { Button } from '@hasura/shared/ui';
import { FaFlask } from 'react-icons/fa';
import { useNavigate } from 'react-router';

export const FeatureFlagFloatingButton = () => {
  const navigate = useNavigate();
  return (
    <Button
      leftIcon={FaFlask}
      className="fixed flex items-center justify-center bottom-4 right-4"
      onClick={() => navigate('/settings/feature-flags')}
    />
  );
};
