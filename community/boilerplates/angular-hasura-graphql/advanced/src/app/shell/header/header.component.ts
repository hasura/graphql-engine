import { Title } from '@angular/platform-browser';
import { Component, OnInit, Input } from '@angular/core';
import { Router } from '@angular/router';
import { MatSidenav } from '@angular/material';

import { AuthenticationService } from '@app/core';

@Component({
  selector: 'app-header',
  templateUrl: './header.component.html',
  styleUrls: ['./header.component.scss']
})
export class HeaderComponent implements OnInit {
  @Input()
  sidenav: MatSidenav;

  constructor(
    private router: Router,
    private titleService: Title,
    private authenticationService: AuthenticationService
  ) {}

  ngOnInit() {}

  logout() {
    this.authenticationService.logout();
  }

  get username(): string {
    const credentials = localStorage.getItem('user_id');
    return credentials;
  }

  get title(): string {
    return this.titleService.getTitle();
  }
}
