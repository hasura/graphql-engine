import React from 'react';

const NotesSection: React.FC<{
  suggestLangChange: boolean;
}> = ({ suggestLangChange }) => {
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
      {suggestLangChange && (
        <li>
          Consider changing custom function language to{' '}
          <a
            href="https://www.postgresql.org/docs/13/plpgsql-structure.html"
            target="_blank"
            rel="noopener noreferrer"
          >
            plpgsql
          </a>
          , as citus doesn&apos;t support <code>sql</code>
        </li>
      )}
    </ul>
  );
};

export default NotesSection;
