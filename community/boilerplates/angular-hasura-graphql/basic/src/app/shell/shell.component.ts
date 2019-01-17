import { Component, OnInit, ViewChild } from '@angular/core';
import { MediaChange, ObservableMedia } from '@angular/flex-layout';
import { MatSidenav } from '@angular/material';
import { filter } from 'rxjs/operators';

@Component({
  selector: 'app-shell',
  templateUrl: './shell.component.html',
  styleUrls: ['./shell.component.scss']
})
export class ShellComponent implements OnInit {
  @ViewChild('sidenav')
  sidenav: MatSidenav;

  constructor(private media: ObservableMedia) {}

  ngOnInit() {
    // Automatically close side menu on screens > sm breakpoint
    this.media
      .asObservable()
      .pipe(filter((change: MediaChange) => change.mqAlias !== 'xs' && change.mqAlias !== 'sm'))
      .subscribe(() => this.sidenav.close());
  }
}
