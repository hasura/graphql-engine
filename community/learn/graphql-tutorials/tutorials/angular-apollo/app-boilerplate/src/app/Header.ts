import {LogoutBtn} from './Auth/LogoutBtn';

import { Component, EventEmitter, Output } from '@angular/core'; 

@Component({  
    selector: 'Header',  
    templateUrl: './Header.template.html',  
    // styleUrls: ['./products.component.css']  
  }) 

export class Header {
  @Output('logoutHandler') logoutHandler: EventEmitter<any> = new EventEmitter();

  logoutHandlerWrapper(item: any) {
      this.logoutHandler.emit(item);
  }
}
