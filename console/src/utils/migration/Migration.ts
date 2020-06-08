import { getDownQueryComments } from './utils';
import { RunSqlType } from '../../types';
// todo use well typed interface after generating
export interface RunSQLQueryType {
  type: string;
  args: object | RunSQLQueryType[];
}

export default class Migration {
  upMigration: RunSQLQueryType[];
  downMigration: RunSQLQueryType[];
  constructor() {
    this.upMigration = [];
    this.downMigration = [];
  }

  hasValue = () => this.upMigration.length > 0;

  // if there is no down migration possible, call the function with argument empty or null
  add = (up: RunSQLQueryType, down: RunSQLQueryType) => {
    this.upMigration = [...this.upMigration, up];
    if (down) this.downMigration = [down, ...this.downMigration];
    // auto generate down query comments based on up queries
    else
      this.downMigration = [
        getDownQueryComments([up as RunSqlType])[0], // reusing the method which works with array
        ...this.downMigration,
      ];
  };

  // this is called when there is only one migration for at that particular index
  UNSAFE_add = (up?: RunSQLQueryType, down?: RunSQLQueryType) => {
    if (up !== undefined && up !== null)
      this.upMigration = [...this.upMigration, up];
    if (down !== undefined && up !== null)
      this.downMigration = [down, ...this.downMigration];
  };
}
