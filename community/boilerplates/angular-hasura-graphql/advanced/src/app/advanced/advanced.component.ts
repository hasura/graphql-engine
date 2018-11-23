import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';

import { Apollo } from 'apollo-angular';
import * as OPERATIONS from '../shared/operations';
import { Observable } from 'rxjs';
import { stringify } from '@angular/compiler/src/util';

@Component({
  selector: 'app-advanced',
  templateUrl: './advanced.component.html',
  styleUrls: ['./advanced.component.scss']
})
export class AdvancedComponent implements OnInit {
  dataSource = [];
  noItemsInList = true;

  constructor(private apollo: Apollo) {}

  ngOnInit() {
    this.getItemsFromDB();
  }

  getItemsFromDB() {
    this.apollo
      .watchQuery<any>({
        query: OPERATIONS.GetQuery
      })
      .valueChanges.subscribe(({ data }) => {
        if (data.todos.length === 0) {
          this.noItemsInList = true;
        } else {
          this.noItemsInList = false;
          this.dataSource = data.todos;
        }
      });
  }

  addItemInDB() {
    this.apollo
      .mutate<any>({
        mutation: OPERATIONS.AddMutation,
        variables: {
          objects: [
            {
              text: 'Congrats! You just added a string.',
              user_id: JSON.parse(localStorage.getItem('credentials')).username,
              updated_at: stringify(Date.now())
            }
          ]
        }
      })
      .subscribe(
        ({ data }) => {
          this.dataSource.push(data.insert_todos.returning[0]);
        },
        error => {
          console.log('Could not add due to ' + error);
        }
      );
  }

  updateItem() {
    const id = this.dataSource[this.dataSource.length - 1].id;
    this.apollo
      .mutate<any>({
        mutation: OPERATIONS.UpdateMutation,
        variables: {
          where: {
            id: {
              _eq: id
            }
          },
          set: {
            text: 'The string just got updated!'
          }
        }
      })
      .subscribe(
        ({ data }) => {
          this.dataSource.pop();
          this.dataSource.push(data.update_todos.returning[0]);
        },
        error => {
          console.log('Could update add due to ' + error);
        }
      );
  }

  deleteItem() {
    const id = this.dataSource[this.dataSource.length - 1].id;
    this.apollo
      .mutate<any>({
        mutation: OPERATIONS.DeleteMutation,
        variables: {
          where: {
            id: {
              _eq: id
            }
          }
        }
      })
      .subscribe(
        ({ data }) => {
          this.dataSource.pop();
        },
        error => {
          console.log('Could delete add due to ' + error);
        }
      );
  }
}
