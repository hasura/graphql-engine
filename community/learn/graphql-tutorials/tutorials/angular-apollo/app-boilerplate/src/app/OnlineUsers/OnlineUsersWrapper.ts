import { Component, OnInit, Input } from '@angular/core';

import {OnlineUser} from "./OnlineUser"

@Component({  
    selector: 'OnlineUsersWrapper',  
    templateUrl: './OnlineUsersWrapper.template.html',  
    // styleUrls: ['./products.component.css']  
  }) 

export class OnlineUsersWrapper {
    onlineUsers = [
        { name: "someUser1" },
        { name: "someUser2" }
      ]
   
}
