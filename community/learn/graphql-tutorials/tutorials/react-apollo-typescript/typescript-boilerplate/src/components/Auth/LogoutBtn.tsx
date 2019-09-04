import * as React from 'react';
import { Button } from 'react-bootstrap';

const LogoutBtn = ({logoutHandler}: {logoutHandler: VoidFunction}) => (
  <Button
    id="qsLogoutBtn"
    className="btn-margin logoutBtn"
    onClick={() => {logoutHandler()}}
  >
    Log Out
  </Button>
);

export default LogoutBtn;
