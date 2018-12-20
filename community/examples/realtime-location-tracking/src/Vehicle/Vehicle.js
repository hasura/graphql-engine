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

    const hasuraImg = 'data:image/png;base64,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';
    // require('../assets/hasura_logo.png');
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
                <img alt="hasura logo" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_black_200px.png" />
              </a>
                &nbsp; | &nbsp;
              <a href="/console" target="_blank" rel="noopener noreferrer">
                Backend
              </a>
              &nbsp; | &nbsp;
              <a href="https://github.com/hasura/graphql-engine/tree/master/community/examples/realtime-location-tracking" target="_blank" rel="noopener noreferrer">
                Source
              </a>
              <div className="footer-small-text">
                <span>
                  (The database resets every 24 hours)
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
