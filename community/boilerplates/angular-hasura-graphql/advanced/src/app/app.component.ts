import { Component, OnInit } from '@angular/core';

import { environment } from '@env/environment';
import { Logger, AuthenticationService } from '@app/core';

const log = new Logger('App');

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent implements OnInit {
  constructor(authenticationService: AuthenticationService) {
    authenticationService.handleAuthentication();
  }

  ngOnInit() {
    // Setup logger
    if (environment.production) {
      Logger.enableProductionMode();
    }

    log.debug('init');
  }
}
