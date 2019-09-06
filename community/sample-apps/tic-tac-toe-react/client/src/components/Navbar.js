import React from 'react';
import { Navbar, Nav } from 'react-bootstrap'
import { getUsername } from '../utils';

const NavBar = () => {
  return (
    <Navbar bg="light" expand="lg" className="margin-bottom" sticky="top">
      <Navbar.Brand href="/">Tic Tac Toe</Navbar.Brand>
      <Navbar.Toggle aria-controls="basic-navbar-nav" />
      <Navbar.Collapse id="basic-navbar-nav">
        <Nav className="mr-auto">
          <Nav.Link href="/">Boards</Nav.Link>
        </Nav>
      </Navbar.Collapse>
      <Nav className="mr-auto">
        <div>
          Your username:&nbsp;&nbsp; <b>{getUsername()}</b>
        </div>
      </Nav>
    </Navbar>
  )
}

export default NavBar;