import { Component, OnInit, Input } from '@angular/core'; 
import auth0 from 'auth0-js';

import {AUTH_CONFIG} from './auth0-variables';
;
@Component({  
  selector: 'Auth0Wrapper',  
  templateUrl: './Auth0Wrapper.template.html',  
})  

export class Auth0Wrapper implements OnInit {
  auth0 = new auth0.WebAuth({
    domain: AUTH_CONFIG.domain,
    clientID: AUTH_CONFIG.clientId,
    redirectUri: AUTH_CONFIG.callbackUrl,
    audience: `https://${AUTH_CONFIG.domain}/userinfo`,
    responseType: 'token id_token',
    scope: 'openid profile'
  });
  isAuthenticated:any = false;// This can be true, false, 'loading'
  idToken = null;
  accessToken;
  expiresAt;
  @Input('location') location: any;

  constructor() {
  }

  login() {
    this.auth0.authorize();
  }

  handleAuthentication = () => {
    this.isAuthenticated= 'loading';

    this.auth0.parseHash((err, authResult) => {
      if (authResult && authResult.accessToken && authResult.idToken) {
        this.setSession(authResult);
      } else if (err) {
        this.logout();
        console.error(err);
        alert(`Error: ${err.error} - ${err.errorDescription}`);
      }
    });
  };

  getAccessToken() {
    return this.accessToken;
  }

  getIdToken() {
    return this.idToken;
  }

  setSession(authResult) {
    // Set isLoggedIn flag in localStorage
    localStorage.setItem('isLoggedIn', 'true');

    // Set the time that the access token will expire at
    let expiresAt = (authResult.expiresIn * 1000) + new Date().getTime();
    this.accessToken = authResult.accessToken;
    this.idToken = authResult.idToken;
    this.expiresAt = expiresAt;
    localStorage.setItem('token', this.idToken);

    // navigate to the home route
    this.isAuthenticated = true;
    this.idToken = authResult.idToken;
  }

  renewSession() {
    this.isAuthenticated = 'loading';

    this.auth0.checkSession({}, (err, authResult) => {
       if (authResult && authResult.accessToken && authResult.idToken) {
         this.setSession(authResult);
       } else if (err) {
         this.logout();
         console.log(err);
         alert(`Could not get a new token (${err.error}: ${err.error_description}).`);
       }
    });
  }

  logout() {
    // Remove tokens and expiry time
    this.accessToken = null;
    this.idToken = null;
    this.expiresAt = 0;

    // Remove isLoggedIn flag from localStorage
    localStorage.removeItem('isLoggedIn');
    localStorage.removeItem('token');

    this.auth0.logout({
      return_to: window.location.origin
    });

    // navigate to the home route
    this.isAuthenticated = false;
    this.idToken = null;
  }

  isExpired () {
    // Check whether the current time is past the
    // access token's expiry time
    let expiresAt = this.expiresAt;
    return new Date().getTime() > expiresAt;
  }

  ngOnInit() {
    // If this is a callback URL then do the right things
    const location = window.location;
    if (location && location.pathname.startsWith('/callback') && /access_token|id_token|error/.test(location.hash)) {
      this.handleAuthentication();
      return;
    }

    // On first load, check if we are already logged in and get the idTokens and things
    if(localStorage.getItem('isLoggedIn') === 'true') {
      this.renewSession();
      return;
    }
  }


}
