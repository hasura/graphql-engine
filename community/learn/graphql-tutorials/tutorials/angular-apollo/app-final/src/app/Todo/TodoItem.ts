import { Component, Input } from '@angular/core';
import { Apollo } from 'apollo-angular'; 
import gql from 'graphql-tag';

import {GET_MY_TODOS} from './TodoPrivateList'

const TOGGLE_TODO = gql`
  mutation toggleTodo ($id: Int!, $isCompleted: Boolean!) {
    update_todos(where: {id: {_eq: $id}}, _set: {is_completed: $isCompleted}) {
      affected_rows
    }
  }
  `;

const REMOVE_TODO = gql`
mutation removeTodo ($id: Int!) {
  delete_todos(where: {id: {_eq: $id}}) {
    affected_rows
  }
}
`;

@Component({  
    selector: 'TodoItem',  
    templateUrl: './TodoItem.template.html',  
  }) 

export class TodoItem {
    @Input('todo') todo: any;
    
    constructor(private apollo: Apollo) {}
    
    removeTodo(e) {
        e.preventDefault();
        e.stopPropagation();
        this.apollo.mutate({
          mutation: REMOVE_TODO,
          variables: {id: this.todo.id},
          optimisticResponse: {},
          update: (cache) => {
            const existingTodos: any = cache.readQuery({ query: GET_MY_TODOS });
            const newTodos = existingTodos.todos.filter(t => (t.id !== this.todo.id));
            cache.writeQuery({
              query: GET_MY_TODOS,
              data: {todos: newTodos}
            });
          },
        }).subscribe(({ data, loading }) => {
          console.log('got data ', data);
        },(error) => {
          console.log('there was an error sending the query', error);
        });
      };
    
      toggleTodo() {
        this.apollo.mutate({
          mutation: TOGGLE_TODO,
          variables: {id: this.todo.id, isCompleted: !this.todo.is_completed},
          update: (cache) => {
            const existingTodos : any = cache.readQuery({ query: GET_MY_TODOS });
            const newTodos = existingTodos.todos.map(t => {
              if (t.id === this.todo.id) {
                return({...t, is_completed: !t.is_completed});
              } else {
                return t;
              }
            });
            cache.writeQuery({
              query: GET_MY_TODOS,
              data: {todos: newTodos}
            });
          },
        }).subscribe(({ data, loading }) => {
          console.log('got data ', data);
        },(error) => {
          console.log('there was an error sending the query', error);
        });
      };
}
