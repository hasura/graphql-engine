import * as React from 'react';
import LogoutBtn from './Auth/LogoutBtn';

const Header = ({logoutHandler}: {logoutHandler: VoidFunction}) => (
  <nav className="m-bottom-0 topNav">
    GraphQL Tutorial App

    <LogoutBtn logoutHandler={logoutHandler} />
  </nav>
);

export default Header;
