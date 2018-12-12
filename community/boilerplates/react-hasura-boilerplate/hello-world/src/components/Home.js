import React, { Component } from 'react';
import '../App.css';

class Home extends Component {
  render() {
    return (
      <div className="container">
        <h1>React-Hasura-Boilerplate</h1>
        <h3>Includes :</h3>
        <ul>
          <li>Apollo-client</li>
          <li>React-Router</li>
          <li>create-react-app v2</li>
          <li>Bootstrap v3.37</li>
          <li>Font-awesome</li>
        </ul>
      </div>
    );
  }
}

export default Home;
