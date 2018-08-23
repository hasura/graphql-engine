import React from 'react';
import './App.css';
import SubscriptionExample from './components/SubscriptionExample';

import {
  BrowserRouter as Router,
  Route,
  Link
} from 'react-router-dom';

const Home = () => (
  <div>
    <h2> Home </h2>
  </div>
);

const RouteOne = () => (
  <div>
    <h2>Route One</h2>
  </div>
);

const RouteTwo = () => (
  <div>
    <h2>Route Two </h2>
  </div>
);

const AppRouter = (props) => {
  return (
    <Router>
      <div>
        { /*
          <ul>
          <li><Link to="/">Home</Link></li>
          <li><Link to="/route-1">Route one</Link></li>
          <li><Link to="/route-2">Route two</Link></li>
          <li><Link to="subscriptions-example"> Subscriptions Example </Link></li>
        </ul>
        */ }
        


        <Route exact path="/" component={SubscriptionExample}/>
        <Route path="/route-1" component={RouteOne}/>
        <Route path="/route-2" component={RouteTwo}/>
        <Route path="/subscriptions-example" component={SubscriptionExample}/>
      </div>
    </Router>
  );
}

export default AppRouter;

















