import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FlexLayoutModule } from '@angular/flex-layout';

import { MaterialModule } from '@app/material.module';
import { BasicRoutingModule } from './basic-routing.module';
import { BasicComponent } from './basic.component';
import { GraphQLModule } from './graphql.module';

@NgModule({
  imports: [CommonModule, FlexLayoutModule, MaterialModule, BasicRoutingModule, GraphQLModule],
  declarations: [BasicComponent]
})
export class BasicModule {}
