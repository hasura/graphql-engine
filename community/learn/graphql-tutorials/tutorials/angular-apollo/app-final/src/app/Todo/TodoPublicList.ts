import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subscription } from 'rxjs';
import { Apollo } from 'apollo-angular';
import gql from 'graphql-tag'; 

 // Run a subscription to get the latest public todo
 const NOTIFY_NEW_PUBLIC_TODOS = gql`
 subscription notifyNewPublicTodos {
   todos (where: { is_public: { _eq: true}}, limit: 1, order_by: {created_at: desc }) {
     id
     created_at
   }
 }
 `;

@Component({  
    selector: 'TodoPublicList',  
    templateUrl: './TodoPublicList.template.html',  
  }) 

export class TodoPublicList implements OnInit, OnDestroy{
          olderTodosAvailable= true;
          newTodosCount = 0;
          oldestTodoId;
          newestTodoId;
          todos= [];
          loading: boolean = true;

      private oldTodosQuerySubscription: Subscription;
      private newTodosQuerySubscription: Subscription;
      private notifyNewTodosQuerySubscription: Subscription;

      constructor(private apollo: Apollo) {}

      ngOnInit() {
        this.getNotifications();
      }

      getNotifications() {
        this.notifyNewTodosQuerySubscription = this.apollo.subscribe({
          query: NOTIFY_NEW_PUBLIC_TODOS,
        }).subscribe(({ data, loading }) => {
          this.loading = loading;
          if(data) {
            const latestTodo = data.todos.length ? data.todos[0] : null;
            this.olderTodosAvailable = latestTodo? true: false;
            this.oldestTodoId=latestTodo? (latestTodo.id +1) : 0 ;
            if (latestTodo && latestTodo.id > this.newestTodoId) {
              this.newestTodoId = latestTodo.id;
              this.newTodosCount = this.newTodosCount +1;
            } else {
              this.newestTodoId=latestTodo? latestTodo.id : 0;
              this.loadOlder();
            }
             
          }
          console.log('got data ', data);
        },(error) => {
          console.log('there was an error sending the query', error);
        });
      }
    
      loadNew() {
        const GET_NEW_PUBLIC_TODOS = gql`
        query getNewPublicTodos ($latestVisibleId: Int!) {
          todos(where: { is_public: { _eq: true}, id: {_gt: $latestVisibleId}}, order_by: { created_at: desc }) {
            id
            title
            created_at
            user {
              name
            }
          }
        }
      `;
      this.newTodosQuerySubscription = this.apollo.watchQuery({
        query: GET_NEW_PUBLIC_TODOS,
        variables: {latestVisibleId: this.todos[0].id}
      })
      .valueChanges
      .subscribe(({ data, loading }) => {
        const todosData : any = data;
        if(todosData) {
          this.newestTodoId = todosData.todos[0].id;
          this.todos = [...todosData.todos, ...this.todos]
          this.newTodosCount=0;
        }
        console.log('got data ', data);
      },(error) => {
        console.log('there was an error sending the query', error);
      });
      }
    
      loadOlder() {
      const GET_OLD_PUBLIC_TODOS = gql`
      query getOldPublicTodos ($oldestTodoId: Int!) {
        todos (where: { is_public: { _eq: true}, id: {_lt: $oldestTodoId}}, limit: 7, order_by: { created_at: desc }) {
          id
          title
          created_at
          user {
            name
          }
        }
      }`;
    this.oldTodosQuerySubscription = this.apollo.watchQuery({
      query: GET_OLD_PUBLIC_TODOS,
      variables: {oldestTodoId: this.oldestTodoId}
    })
    .valueChanges
    .subscribe(({ data, loading }) => {
      const todosData : any = data;
      if(todosData) {
        if (todosData.todos.length) {
          this.oldestTodoId = todosData.todos[todosData.todos.length - 1].id;
          this.todos = [...this.todos, ...todosData.todos]
        } else {
          this.olderTodosAvailable = false;
        }
      }
      console.log('got data ', data);
    },(error) => {
      console.log('there was an error sending the query', error);
    });
      }

   ngOnDestroy() {
    this.notifyNewTodosQuerySubscription.unsubscribe();
    this.newTodosQuerySubscription.unsubscribe();
    this.oldTodosQuerySubscription.unsubscribe();
   }   
    
}
