import { Component, OnInit, Input } from '@angular/core';
import { Apollo } from 'apollo-angular';
import gql from 'graphql-tag';

import {GET_MY_TODOS} from './TodoPrivateList';

const ADD_TODO = gql `
  mutation ($todo: String!, $isPublic: Boolean!) {
    insert_todos(objects: {title: $todo, is_public: $isPublic}) {
      affected_rows
      returning {
        id
        title
        created_at
        is_completed
      }
    }
  }
 `;

@Component({  
    selector: 'TodoInput',  
    templateUrl: './TodoInput.template.html',  
  }) 

export class TodoInput {
    @Input('isPublic') isPublic: any = false;
    todoInput: any= '';
    loading: boolean = true;
    
    constructor(private apollo: Apollo) {}

    addTodo(e) {
      e.preventDefault();
      this.apollo.mutate({
        mutation: ADD_TODO,
        variables: {
          todo: this.todoInput,
          isPublic: this.isPublic 
        },
        update: (cache, {data}) => {
          
          if(this.isPublic) return null;
          
          const existingTodos : any = cache.readQuery({
            query: GET_MY_TODOS
          });

          const newTodo = data.insert_todos.returning[0];
          cache.writeQuery({
            query: GET_MY_TODOS,
            data: {todos: [newTodo, ...existingTodos.todos]}
          });
        },
        
      }).subscribe(({ data, loading }) => {
        this.loading = loading;
        this.todoInput = '';
        console.log('got data ', data);
      },(error) => {
        console.log('there was an error sending the query', error);
      });
    }
}
