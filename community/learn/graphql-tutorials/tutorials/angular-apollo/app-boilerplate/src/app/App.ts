import { Component, Output, EventEmitter } from '@angular/core';

@Component({
  selector: 'App',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class App {
  @Output('logout') logout: EventEmitter<any> = new EventEmitter();

  logoutHandler(item: any) {
      this.logout.emit(item);
  }
  
}
