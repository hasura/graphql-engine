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

  hasValue = () => this.upMigration.length > 0 || this.downMigration.length > 0;

  // if there is no down migration possible, call the function with argument empty or null
  add = (up?: RunSQLQueryType, down?: RunSQLQueryType) => {
    if (up !== undefined && up !== null)
      this.upMigration = [...this.upMigration, up];
    if (down !== undefined && up !== null)
      this.downMigration = [down, ...this.downMigration];
  };
}
