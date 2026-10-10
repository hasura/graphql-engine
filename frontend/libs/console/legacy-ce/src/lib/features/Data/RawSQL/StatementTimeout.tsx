import { FC } from 'react';
import { IconTooltip, Input } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

const StatementTimeout: FC<StatementTimeoutProps> = ({
  isMigrationChecked,
  statementTimeout,
  updateStatementTimeout,
  isCliMode,
}) => {
  return (
    <Flex className="mt-3" align="center" gap="2">
      Statement timeout (seconds)
      <IconTooltip message="Abort statements that take longer than the specified time" />
      <Input
        disabled={isCliMode && isMigrationChecked}
        title={
          isMigrationChecked
            ? 'Setting statement timeout is not supported for migrations'
            : ''
        }
        min={0}
        value={statementTimeout || ''}
        type="number"
        data-test="raw-sql-statement-timeout"
        onChange={(event) => updateStatementTimeout(event.target.value)}
      />
    </Flex>
  );
};

export default StatementTimeout;

interface StatementTimeoutProps {
  isCliMode: boolean;
  isMigrationChecked: boolean;
  statementTimeout: number;
  updateStatementTimeout: (e: string) => void;
}
