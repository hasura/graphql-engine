import { Text } from '@hasura/shared/ui';
import { Code, Link } from '@radix-ui/themes';
import React from 'react';

const NotesSection: React.FC<{
  suggestLangChange: boolean;
}> = ({ suggestLangChange }) => {
  return (
    <ul className="list-item">
      <li>
        <Text>
          You can create views, alter tables or just about run any SQL
          statements directly on the database.
        </Text>
      </li>
      <li>
        <Text>
          Multiple SQL statements can be separated by semicolons, <Code>;</Code>
          , however, only the result of the last SQL statement will be returned.
        </Text>
      </li>
      <li>
        <Text>
          Multiple SQL statements will be run as a transaction. i.e. if any
          statement fails, none of the statements will be applied.
        </Text>
      </li>
      {suggestLangChange && (
        <li>
          Consider changing custom function language to{' '}
          <Link
            href="https://www.postgresql.org/docs/13/plpgsql-structure.html"
            target="_blank"
            rel="noopener noreferrer"
          >
            plpgsql
          </Link>
          , as citus doesn&apos;t support <Code>sql</Code>
        </li>
      )}
    </ul>
  );
};

export default NotesSection;
