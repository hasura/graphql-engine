import * as React from 'react';
// import {Nav, Navbar, NavItem} from 'react-bootstrap';
import LogoutBtn from './Auth/LogoutBtn';

const Header = ({logoutHandler}: {logoutHandler: VoidFunction}) => (
  /*
  <Navbar fluid className="m-bottom-0">
    <Navbar.Header className="navHeader">
      <Navbar.Brand className="navBrand">
        GraphQL Tutorial App
      </Navbar.Brand>

      <Nav pullRight>
        <NavItem>
          <LogoutBtn logoutHandler={logoutHandler} />
        </NavItem>
      </Nav>
    </Navbar.Header>
  </Navbar>
  */
  <nav className="m-bottom-0">
        GraphQL Tutorial App

          <LogoutBtn logoutHandler={logoutHandler} />
  </nav>
);

export default Header;
