import { Component, OnInit, Input } from '@angular/core'; 

@Component({  
    selector: 'OnlineUser',  
    templateUrl: './OnlineUser.template.html',  
    // styleUrls: ['./products.component.css']  
  }) 

export class OnlineUser {
    @Input('user') user: any;  
}
