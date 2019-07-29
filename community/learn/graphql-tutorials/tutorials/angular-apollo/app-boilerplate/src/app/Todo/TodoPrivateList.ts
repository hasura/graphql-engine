import { Component, OnInit, Input } from '@angular/core';

@Component({  
    selector: 'TodoPrivateList',  
    templateUrl: './TodoPrivateList.template.html',  
  }) 

export class TodoPrivateList implements OnInit {
    
          filter = "all";
          clearInProgress= false;
          todos= [
            {
              id: "1",
              title: "This is private todo 1",
              is_completed: true,
              is_public: false
            },
            {
              id: "2",
              title: "This is private todo 2",
              is_completed: false,
              is_public: false
            }
          ];
          filteredTodos: any;

          ngOnInit() {
            this.filteredTodos = this.todos; 
             
          }
    
      filterResults($event) {
        this.filter = $event.filter;
        this.filteredTodos = this.todos; 
        if (this.filter === "active") {
            this.filteredTodos = this.todos.filter(todo => todo.is_completed !== true);
          } else if (this.filter === "completed") {
            this.filteredTodos = this.todos.filter(todo => todo.is_completed === true);
          }
      }
    
      clearCompleted() {}
}
