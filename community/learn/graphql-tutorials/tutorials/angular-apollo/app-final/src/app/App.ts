import { Component, Output, OnInit, EventEmitter } from '@angular/core';
import {Header} from './Header';
import {TodoPrivateWrapper} from './Todo/TodoPrivateWrapper';
import {TodoPublicWrapper} from './Todo/TodoPublicWrapper';
import {OnlineUsersWrapper} from './OnlineUsers/OnlineUsersWrapper';
import ApolloClient from 'apollo-client';
import { InMemoryCache } from 'apollo-cache-inmemory';
import { HttpLink } from 'apollo-angular-link-http';
import { Apollo } from 'apollo-angular';

@Component({
  selector: 'App',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class App implements OnInit {
  @Output('logout') logout: EventEmitter<any> = new EventEmitter();

  logoutHandler(item: any) {
      this.logout.emit(item);
  }
  
  ngOnInit() {
    console.log("jasdjsada");
  }
}
