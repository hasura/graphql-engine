import { Component, OnInit, Input } from '@angular/core'; 

@Component({  
    selector: 'TodoItem',  
    templateUrl: './TodoItem.template.html',  
  }) 

export class TodoItem {
    @Input('todo') todo: any;
    
    removeTodo = (e) => {
        e.preventDefault();
        e.stopPropagation();
      };
    
      toggleTodo = () => {};
}
