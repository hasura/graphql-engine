import { Component, Input } from '@angular/core'; 

@Component({  
    selector: 'TaskItem',  
    templateUrl: './TaskItem.template.html',  
  }) 

export class TaskItem {
    @Input('todo') todo: any;  
}
