import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core'; 

@Component({  
    selector: 'Login',  
    templateUrl: './Login.template.html',  
    // styleUrls: ['./products.component.css']  
})  

export class Login {
    @Output('loginHandler') loginHandler: EventEmitter<any> = new EventEmitter();

    loginHandlerWrapper(item: any) {
        this.loginHandler.emit(item);
    }
} 

