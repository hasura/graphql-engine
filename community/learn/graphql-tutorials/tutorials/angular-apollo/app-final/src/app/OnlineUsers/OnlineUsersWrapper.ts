import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subscription } from 'rxjs';
import { Apollo } from 'apollo-angular';
import gql from 'graphql-tag';

const SUBSCRIBE_TO_ONLINE_USERS = gql`
subscription getOnlineUsers {
  online_users(order_by: {user: {name: asc }}) {
    id
    user {
      name
    }
  }
}`

@Component({  
    selector: 'OnlineUsersWrapper',  
    templateUrl: './OnlineUsersWrapper.template.html',  
  }) 

export class OnlineUsersWrapper implements OnInit, OnDestroy {
    onlineUsers = []
    onlineIndicator: any;
    loading: boolean = true;

    private querySubscription: Subscription;

    constructor(private apollo: Apollo) {}

      ngOnInit(){
        this.onlineIndicator = setInterval(() => this.updateLastSeen(), 30000);
        this.querySubscription = this.apollo.subscribe({
          query: SUBSCRIBE_TO_ONLINE_USERS,
        }).subscribe(({ data, loading }) => {
          if(data) {
            const users = data.online_users;
            this.loading = loading;
            this.onlineUsers = [];
              users.forEach((u, index) => {
                this.onlineUsers.push(u.user)
              })
          }
          console.log('got data ', data);
        },(error) => {
          console.log('there was an error sending the query', error);
        }); 
      }

      updateLastSeen() {
        // Use the apollo client to run a mutation to update the last_seen value
        const UPDATE_LASTSEEN_MUTATION=gql`
          mutation updateLastSeen ($now: timestamptz!) {
            update_users(where: {}, _set: {last_seen: $now}) {
              affected_rows
            }
          }`;
        this.apollo.mutate({
          mutation: UPDATE_LASTSEEN_MUTATION,
          variables: {now: (new Date()).toISOString()}
        }).subscribe(({ data, loading }) => {
          console.log('got data ', data);
        },(error) => {
          console.log('there was an error sending the query', error);
        });
      }

      ngOnDestroy() {
        this.querySubscription.unsubscribe();
      }
   
}
