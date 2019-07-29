
import { Component, EventEmitter, Output } from '@angular/core'; 

@Component({  
    selector: 'Header',  
    templateUrl: './Header.template.html',  
  }) 

export class Header {
  @Output('logoutHandler') logoutHandler: EventEmitter<any> = new EventEmitter();

  logoutHandlerWrapper(item: any) {
      this.logoutHandler.emit(item);
  }
}
