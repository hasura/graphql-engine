import { Component, EventEmitter, Output } from '@angular/core'; 

@Component({  
    selector: 'LogoutBtn',  
    templateUrl: './Logout.template.html',  
  }) 

export class LogoutBtn {
  @Output('logoutHandler') logoutHandler: EventEmitter<any> = new EventEmitter();

  logoutHandlerWrapper(item: any) {
      this.logoutHandler.emit(item);
  }
}
