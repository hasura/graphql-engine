import React, { Component } from 'react';

import GoogleMapReact from 'google-map-react';

import { fitBounds } from 'google-map-react/utils';

import {
  GOOGLE_API_KEY,
  bounds,
  HASURA_LOCATION
} from '../constants'

import drivingLoc from '../mapInfo/drivingJson';

class Marker extends Component {
  render() {
    const greatPlaceStyle = {
      position: 'absolute',
      top: "100%",
      left: "50%",
      transform: 'translate(-50%, -50%)'
    };
    const divStyle = {
      'background':'#ffff00',
      'borderRadius':'50%',
      'border': '3px solid black',
      'padding': '2px',
      'height':'30px',
      'width':'30px',
      'position':'relative',
    };
    const imgStyle = {
      'width': '100%',
    };
    const imgSrc = require('../assets/hasura.png');
    return (
      <div style={ greatPlaceStyle }>
        <div style={ divStyle }>
          <img style={ imgStyle } src={ imgSrc } alt="hasura logo location"/>
        </div>
      </div>
    );
  }
}

export class MapContainer extends Component {
  constructor(props) {
    super(props);
    this.state = {};
    this.state.mapLoaded = false;
    this.state.latlng = {};
    if ( this.props.marker_location ) {
      this.state.latlng = this.getLatLng(this.props.marker_location);
    }
  }
	handleGoogleMapApi = (google) => {
    this.setState({ ...this.state, ...google, mapLoaded: true});
    const getPolyline = (routeJson) => {
      var polyline = new google.maps.Polyline({
        path: [],
        strokeColor: '#0000FF',
        strokeWeight: 8
      });
      var bounds = new google.maps.LatLngBounds();

      var legs = routeJson.routes[0].legs;
      for (var i = 0; i < legs.length; i++) {
        var steps = legs[i].steps;
        for (var j = 0; j < steps.length; j++) {
          var nextSegment = steps[j].path;
          for (var k = 0; k < nextSegment.length; k++) {
            /* Polyline requires a latLng object which has .lat() and .lng() methods */
            polyline.getPath().push(new google.maps.LatLng(nextSegment[k]));
            bounds.extend(new google.maps.LatLng(nextSegment[k]));
          }
        }
      }

      polyline.setMap(google.map);
    }
    getPolyline(drivingLoc);
	}
  componentWillReceiveProps(nextProps) {
    if ( nextProps.marker_location !== this.props.marker_location ){
      const nextPos = this.getLatLng(nextProps.marker_location);
      const currPos = this.getLatLng(this.props.marker_location);
      const newLatLng = {...currPos};
      const currThis = this;
			var numDeltas = 100;
			var delay = 10; //milliseconds
			var i = 0;
			var deltaLat;
			var deltaLng;
      transition(nextPos);

			function transition(result){
				i = 0;
				deltaLat = (nextPos.lat - currPos.lat)/numDeltas;
				deltaLng = (nextPos.lng - currPos.lng)/numDeltas;
				moveMarker();
			}

			function moveMarker(){
        newLatLng.lat += deltaLat;
        newLatLng.lng += deltaLng;
        currThis.setState({ ...currThis.state, latlng: { ...newLatLng }});
				if(i!==numDeltas){
					i++;
					setTimeout(moveMarker, delay);
				}
			} 
    }
  }
  getLatLng(pos) {
    if (pos) {
      const markerLocationSplit = pos.replace(/[()]/g, "").split(",").map(x => x.trim());

      const markerLocation = {
        lat: parseFloat(markerLocationSplit[0]),
        lng: parseFloat(markerLocationSplit[1])
      };
      return markerLocation;
    }
    return HASURA_LOCATION;
  }
  render() {
    const size = {
      width: 320, // map width in pixels
      height: 400, // map height in pixels
    };
    const {center, zoom} = fitBounds(bounds, size);
    return (
      <div style={{height: '100%'}}>
        <GoogleMapReact
          bootstrapURLKeys={
            {
              key: GOOGLE_API_KEY
            }
          }
          center={center} 
          zoom={zoom}
          yesIWantToUseGoogleMapApiInternals
          onGoogleApiLoaded={this.handleGoogleMapApi}
        > 
          <Marker lat={this.state.latlng.lat ? this.state.latlng.lat : HASURA_LOCATION.lat} lng={this.state.latlng.lng ? this.state.latlng.lng : HASURA_LOCATION.lng } />
        </GoogleMapReact>
      </div>
    );
  }
}

export default MapContainer;
