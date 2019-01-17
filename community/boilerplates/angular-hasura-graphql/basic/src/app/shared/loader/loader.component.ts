import { Component, OnInit, Input } from '@angular/core';

@Component({
  selector: 'app-loader',
  templateUrl: './loader.component.html',
  styleUrls: ['./loader.component.scss']
})
export class LoaderComponent implements OnInit {
  @Input()
  isLoading = false;
  @Input()
  size = 1;
  @Input()
  message: string;

  constructor() {}

  ngOnInit() {}
}
