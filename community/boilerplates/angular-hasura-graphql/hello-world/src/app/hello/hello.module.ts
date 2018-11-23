import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { HelloRoutingModule } from './hello-routing.module';
import { GraphQLModule } from './graph-ql.module';

@NgModule({
  imports: [
    CommonModule,
    HelloRoutingModule,
    GraphQLModule
  ],
  declarations: []
})
export class HelloModule { }
