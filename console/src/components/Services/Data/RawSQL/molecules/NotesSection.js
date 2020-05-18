import React from 'react';

/**
 * # NoteSection React static component
 *
 * @returns
 */
const NotesSection = () => {
  return (
    <ul>
      <li>
        You can create views, alter tables or just about run any SQL statements
        directly on the database.
      </li>
      <li>
        Multiple SQL statements can be separated by semicolons, <code>;</code>,
        however, only the result of the last SQL statement will be returned.
      </li>
      <li>
        Multiple SQL statements will be run as a transaction. i.e. if any
        statement fails, none of the statements will be applied.
      </li>
    </ul>
  );
};

export default NotesSection;
