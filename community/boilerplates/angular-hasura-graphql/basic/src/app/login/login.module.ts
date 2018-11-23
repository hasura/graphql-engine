import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReactiveFormsModule } from '@angular/forms';
import { FlexLayoutModule } from '@angular/flex-layout';

import { SharedModule } from '@app/shared';
import { MaterialModule } from '@app/material.module';
import { LoginRoutingModule } from './login-routing.module';
import { LoginComponent } from './login.component';

@NgModule({
  imports: [CommonModule, ReactiveFormsModule, SharedModule, FlexLayoutModule, MaterialModule, LoginRoutingModule],
  declarations: [LoginComponent]
})
export class LoginModule {}
