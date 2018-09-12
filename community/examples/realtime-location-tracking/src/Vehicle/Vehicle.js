import React, {Component} from 'react';

import { ApolloConsumer, Subscription } from 'react-apollo';
import gql from 'graphql-tag';

import client from '../apollo'
// import { httpurl } from '../constants';
import { ApolloProvider } from 'react-apollo';

import uuidv4 from 'uuid/v4';
import App from '../App/App';
import UserInfo from '../UserInfo/UserInfo';
import locationData from '../mapInfo/location';
import './Vehicle.css';

class Vehicle extends Component { constructor() {
    super();
    this.state = {};
    this.loadVehicleInfo = this.loadVehicleInfo.bind(this);
    this.state.vehicleInfo = {};
    this.state.vehicleId = uuidv4();
    this.state.startTracking = false;
    this.state.delay = 3000;
    this.state.locationId = 0;
    this.state.pollId = -1;
    this.state.isLoading = false;
  }
  updateLocation() {
    if (locationData.length === this.state.locationId) {
      this.setState({ ...this.state, locationId: 0 });
    }
    const insert_vehicle_location = gql`
      mutation insert_vehicle_location ($objects: [vehicle_location_insert_input!]! )  {
        insert_vehicle_location (objects: $objects){
          returning {
            id
          }
        }
      }
    `;
    const variables = {
      "objects": [
        {
          "vehicle_id": this.state.vehicleId,
          "location": locationData[this.state.locationId],
        }
      ]
    };
    this.props.client.mutate(
      {
        mutation: insert_vehicle_location,
        variables: { ...variables },
      }
    ).then((response) => {
      this.setState({ ...this.state, locationId: this.state.locationId + 1});

    })
    .catch((error) => console.error(error));
  }
  loadVehicleInfo(e) {
    const vehicleId = e.target.getAttribute('data-vehicle-id');
    if ( vehicleId ){
      this.setState({ ...this.state, vehicleId: parseInt(vehicleId, 10)});
    }
  }
  handleTrackLocationClick() {
    this.setState({ ...this.state, isLoading: true });
    const insert_vehicle = gql`
      mutation insert_vehicle ($objects: [vehicle_insert_input!]! )  {
        insert_vehicle (objects: $objects){
          returning {
            id
          }
        }
      }
    `;
    const variables = {
      "objects": [
        {
          "id": this.state.vehicleId,
          "name": this.state.vehicleId,
        }
      ]
    };
    this.props.client.mutate(
      {
        mutation: insert_vehicle,
        variables: { ...variables },
      }
    ).then((response) => {
      this.setState({ ...this.state, startTracking: true });
      const pollId = setInterval(this.updateLocation.bind(this), this.state.delay);
      this.setState({ ...this.state, pollId: pollId, isLoading: false });
    }).catch((error) => {
      this.setState({ ...this.state, isLoading: false });
      console.error(error)
    });
  }
  componentWillUnmount() {
    clearInterval(this.state.pollId);
  }
  render() {
    const GET_USERS = gql`
        subscription getVehicle($vehicleId: String!) {
          vehicle (where: { id: { _eq: $vehicleId }}) {
            id
            name
          }
        }
    `;

    const hasuraImg = require('../assets/hasura_logo.png');
    return (
      <div className="App">
        <header className="App-header">
          <h1 className="App-title">Realtime location tracking example</h1>
        </header>
        <div className="container">
          { this.state.startTracking ?
            [
              <ApolloConsumer key={'1'}>
                {client => (
                  <Subscription subscription={GET_USERS} variables={{ vehicleId: this.state.vehicleId }}>
                    {({ loading, error, data }) => {
                      if (loading) return <p>Loading...</p>;
                      if (error) return <p>Error!</p>;

                      return (
                        <div className="list_of_vehicles">
                          <div>
                            <b>Vehicle ID</b>: { this.state.vehicleId }
                          </div>
                          <div className="vehicle_info">
                            (This vehicle is generating live location events and sending them to the database.)
                          </div>
                        </div>
                      );
                    }}
                  </Subscription>
                )}
              </ApolloConsumer>,
              <App key='2' vehicleId={ this.state.vehicleId } />
            ]
            :
            <UserInfo userId={ this.state.vehicleId } handleTrackLocationClick={ this.handleTrackLocationClick.bind(this) } isLoading={ this.state.isLoading }/>
          }
        </div>
        { this.state.startTracking ? (
          <footer className="Vehicle-footer displayFlex">
            <div className="container hasura-logo">
              <a href="https://hasura.io" target="_blank" rel="noopener noreferrer">
                Powered by <img src={ hasuraImg } alt="hasura logo" />
              </a>
                &nbsp; | &nbsp;
              <a href="https://realtime-backend.herokuapp.com/console/data/schema/public" target="_blank" rel="noopener noreferrer">
                Database
              </a>
              &nbsp; | &nbsp;
              <a href="https://github.com/hasura/realtime-location-app" target="_blank" rel="noopener noreferrer">
                Source
              </a>
              <div className="footer-small-text">
                <span>
                  (The database resets every 30 minutes)
                </span>
              </div>
            </div>
          </footer>
        ) : null }
      </div>
    );
  }
}

const ApolloWrappedComponent = () => {
  return (
    <ApolloProvider client={client}>
      <Vehicle client={ client }/>
    </ApolloProvider>
  );
};

export default ApolloWrappedComponent;
