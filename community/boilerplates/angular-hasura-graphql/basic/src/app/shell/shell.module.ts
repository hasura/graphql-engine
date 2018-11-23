import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { FlexLayoutModule } from '@angular/flex-layout';
import { MaterialModule } from '@app/material.module';

import { ShellComponent } from './shell.component';
import { HeaderComponent } from './header/header.component';

@NgModule({
  imports: [CommonModule, FlexLayoutModule, MaterialModule, RouterModule],
  declarations: [HeaderComponent, ShellComponent]
})
export class ShellModule {}
