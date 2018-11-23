import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FlexLayoutModule } from '@angular/flex-layout';

import { MaterialModule } from '@app/material.module';
import { AdvancedRoutingModule } from './advanced-routing.module';
import { AdvancedComponent } from './advanced.component';
import { GraphQLModule } from './graphql.module';

@NgModule({
  imports: [
    CommonModule,
    FlexLayoutModule,
    MaterialModule,
    AdvancedRoutingModule,
    GraphQLModule
  ],
  declarations: [AdvancedComponent]
})
export class AdvancedModule {}
