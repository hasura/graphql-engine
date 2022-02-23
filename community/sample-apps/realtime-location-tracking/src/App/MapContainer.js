import React, { useState, useEffect } from 'react';
import GoogleMapReact from 'google-map-react';
import { fitBounds } from 'google-map-react';
import {
  GOOGLE_API_KEY,
  bounds,
  HASURA_LOCATION
} from '../constants'
import drivingLoc from '../mapInfo/drivingJson';
import { usePrevious } from '../hooks/usePrevious';

function Marker() {
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
      'padding': '4px',
      'height':'35px',
      'width':'35px',
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

function MapContainer(props) {
  const markerLocation_initialState = props.marker_location ? getLatLng(props.marker_location) : {};

  const [ mapLoaded, setMapLoaded ] = useState(false);
  const [ latlng, setLatLng ] = useState(markerLocation_initialState);
  const prevMarker_location = usePrevious(props.marker_location);

  const handleGoogleMapApi = (google) => {
    setMapLoaded(true);

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

  useEffect(() => {
    const currPos = getLatLng(prevMarker_location);
    const nextPos = getLatLng(props.marker_location);
    const newLatLng = {...currPos};
    var numDeltas = 100;
    var delay = 10;
    var i = 0;
    var deltaLat;
    var deltaLng

    transition(nextPos);

    function transition(result) {
      i = 0;
      deltaLat = (nextPos.lat - currPos.lat) / numDeltas;
      deltaLng = (nextPos.lng - currPos.lng) / numDeltas;

      moveMarker();
    }

    function moveMarker() {
      newLatLng.lat += deltaLat;
      newLatLng.lng += deltaLng;

      setLatLng({ ...newLatLng });

      if(i!==numDeltas){
        i++;
        setTimeout(moveMarker, delay);
      }
    }
  }, [props.marker_location]);

  function getLatLng(pos) {
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

  const size = {
    width: 320, // map width in pixels
    height: 400, // map height in pixels
  };

  const { center, zoom } = fitBounds(bounds, size);

  return (
    <div style={{ height: '100%' }}>
      <GoogleMapReact
        bootstrapURLKeys={
          {
            key: GOOGLE_API_KEY
          }
        }
        center={ center } 
        zoom={ zoom }
        yesIWantToUseGoogleMapApiInternals
        onGoogleApiLoaded={ handleGoogleMapApi }
      > 
        <Marker lat={ latlng.lat ? latlng.lat : HASURA_LOCATION.lat} lng={latlng.lng ? latlng.lng : HASURA_LOCATION.lng } />
      </GoogleMapReact>
    </div>
  );
}

export default MapContainer;
