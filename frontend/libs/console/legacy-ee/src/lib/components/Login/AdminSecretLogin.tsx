import React from 'react';
import { Button } from '@hasura/shared/ui';
import { FaAngleLeft } from 'react-icons/fa6';
import { AdminSecretLoginForm } from '@hasura/console-legacy-ce';

export const AdminSecretLogin = ({
  backToLoginHome,
  children,
}: {
  children?: React.ReactNode;
  backToLoginHome: React.MouseEventHandler<HTMLButtonElement>;
}) => {
  return (
    <AdminSecretLoginForm>
      {children}
      {backToLoginHome && (
        <Button
          variant="ghost"
          type="button"
          onClick={backToLoginHome}
          leftIcon={FaAngleLeft}
          size="1"
        >
          Back to SSO Sign In
        </Button>
      )}
    </AdminSecretLoginForm>
  );
};
