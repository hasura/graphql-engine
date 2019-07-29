import { Component, OnInit, Input } from '@angular/core'; 

@Component({  
    selector: 'TaskItem',  
    templateUrl: './TaskItem.template.html',  
    // styleUrls: ['./products.component.css']  
  }) 

export class TaskItem {
    @Input('todo') todo: any;  
}
