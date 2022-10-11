import React, { useEffect, useState } from 'react';
import { ApolloProvider, ApolloConsumer, gql } from '@apollo/client';
import { Subscription } from '@apollo/client/react/components';
import PropTypes from 'prop-types';
import GoogleApiWrapper from "./MapContainer";
import client from '../apollo'
import './App.css';

const LOCATION_SUBSCRIPTION = gql`
    subscription getLocation($vehicleId: String!) {
        vehicle(where: {id: {_eq: $vehicleId}}) {
            locations(order_by: {timestamp:desc}, limit: 1) {
                location
                timestamp
            }
        }
    }
`;

function App(props) {
  const [ vehicleId, setVehicleId ] = useState(props.vehicleId)

  useEffect(() => {
    setVehicleId(props.vehicleId)
  }, [props.vehicleId]);

  const queryImg = require('../assets/carbon.png');

  return (
    <ApolloConsumer>
      { client => (
        <Subscription subscription={ LOCATION_SUBSCRIPTION } variables={{ vehicleId }}>
          {({ loading, error, data }) => {
            if (loading) return <p>Loading...</p>;
            if (error) return <p>Error! </p>;

            let latestLocation = null;
            const vehicle = data.vehicle[0];
            const latestLocationObject = vehicle.locations[0];

            if (latestLocationObject) {
              latestLocation = latestLocationObject.location;
            }

            const vehicleLocation = {
              'width': '100%',
              'marginBottom': '20px',
            };

            const queryImgStyle = {
              'width': '100%',
            };
            
            return (
              <div style={ vehicleLocation }>
                <div className="row ">
                  <div className="col-md-6 col-xs-12 request_block">
                    <div className="subscription_wrapper">
                      <h4>Live query</h4>
                      <div className="subscription_query">
                        The GraphQL subscription required to fetch the realtime location data.
                      </div>
                      <div>
                        <img style={ queryImgStyle } src={ queryImg } alt="Subscription query"/>
                      </div>
                    </div>
                  </div>
                  <div className="col-md-6 col-xs-12">
                    <h4>Live tracking</h4>
                    <div className="tracking_info">
                      Location is updated every 3 secs to simulate live tracking
                    </div>
                    <div className="map_wrapper">
                      <GoogleApiWrapper marker_location={ latestLocation }/>
                    </div>
                  </div>
                </div>
              </div>
            );
          }}
        </Subscription>
      )}
    </ApolloConsumer>
  );
}

App.propTypes = {
  vehicleId: PropTypes.string.isRequired,
};

const ApolloWrappedComponent = (props) => {
  return (
    <ApolloProvider client={ client }>
      <App { ...props }/>
    </ApolloProvider>
  );
};

export default ApolloWrappedComponent;
