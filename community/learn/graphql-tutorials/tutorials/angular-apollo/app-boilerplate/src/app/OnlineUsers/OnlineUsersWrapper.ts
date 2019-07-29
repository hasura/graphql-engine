import { Component } from '@angular/core';

@Component({  
    selector: 'OnlineUsersWrapper',  
    templateUrl: './OnlineUsersWrapper.template.html',  
  }) 

export class OnlineUsersWrapper {
    onlineUsers = [
        { name: "someUser1" },
        { name: "someUser2" }
      ]
   
}
