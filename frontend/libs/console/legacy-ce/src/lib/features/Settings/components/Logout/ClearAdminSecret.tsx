import { Button } from '@hasura/shared/ui';
import { hasuraToast } from '@hasura/shared/ui';
import { useNavigate } from 'react-router';
import { useAuthContext } from '@hasura/shared/context';

const ClearAdminSecret = () => {
  const navigate = useNavigate();
  const { logout } = useAuthContext();
  const handleClick: React.MouseEventHandler<HTMLButtonElement> = (e) => {
    e.preventDefault();
    logout();
    hasuraToast({
      type: 'success',
      title: 'Cleared admin-secret',
    });
    navigate('/login');
  };

  return (
    <Button
      mode="default"
      data-test="data-clear-access-key"
      loadingText="Clearing..."
      onClick={handleClick}
    >
      Logout (clear admin-secret)
    </Button>
  );
};

export default ClearAdminSecret;
