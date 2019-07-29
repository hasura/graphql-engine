import { Component, OnInit, Input } from '@angular/core'; 

@Component({  
    selector: 'TodoInput',  
    templateUrl: './TodoInput.template.html' 
  }) 

export class TodoInput {
    @Input('isPublic') isPublic: any = false;
    
    addTodo(e) {
      e.preventDefault();
    }
}
