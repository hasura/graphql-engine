import React from 'react';
import { Navbar, Nav, NavDropdown, Button } from 'react-bootstrap';

const Bar = () => {
  return (
    <Navbar bg="light" fixed="top">
      <Navbar.Brand href="/">graphql2chartjs examples</Navbar.Brand>
      <Navbar.Collapse id="basic-navbar-nav">
        <Nav>
          <NavDropdown title="Jump" id="basic-nav-dropdown">
            <NavDropdown.Item href="#bar">Basic bar chart</NavDropdown.Item>
            <NavDropdown.Item href="#styled-bar">Styled bar chart</NavDropdown.Item>
            <NavDropdown.Item href="#multi-bar">Bar (multiple datasets)</NavDropdown.Item>
            <NavDropdown.Item href="#mixed">Mixed chart (bar and line)</NavDropdown.Item>
            <NavDropdown.Item href="#live-chart">Live chart</NavDropdown.Item>
            <NavDropdown.Item href="#timeseries-chart">Time series</NavDropdown.Item>
          </NavDropdown>
        </Nav>
        <Nav>
          <Nav.Link href="https://github.com/hasura/graphql-engine/tree/master/community/tools/graphql2chartjs">
            <Button variant="dark" size="sm">GitHub</Button>
          </Nav.Link>
          <Nav.Link href="https://codesandbox.io/s/p2wpj1o8pj">
            <Button variant="link" size="sm">Edit in sandbox</Button>
          </Nav.Link>
        </Nav>
      </Navbar.Collapse>
    </Navbar>
  )
}

export default Bar;

