import { Component, Input } from '@angular/core'; 

@Component({  
    selector: 'OnlineUser',  
    templateUrl: './OnlineUser.template.html',  
  }) 

export class OnlineUser {
    @Input('user') user: any;  
}
