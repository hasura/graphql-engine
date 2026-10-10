import { EventTrigger } from '@hasura/shared/types';
import type { RetryConf } from '../../types';
import CommonRetryConf from '../form/RetryConfEditor';
import {
  ExpandableEditor,
  ExpandableEditorFunction,
  Table,
  Text,
} from '@hasura/shared/ui';
import { Heading } from '@radix-ui/themes';

type RetryConfEditorProps = {
  currentTrigger: EventTrigger;
  conf: RetryConf;
  setRetryConf: (r: RetryConf) => void;
  save: ExpandableEditorFunction;
};

const RetryConfEditor = ({
  currentTrigger,
  conf,
  setRetryConf,
  save,
}: RetryConfEditorProps) => {
  const existingConf = currentTrigger.retry_conf;

  const reset = () => {
    setRetryConf(existingConf);
  };

  const collapsed = () => (
    <Table.Root variant="surface">
      <Table.Body>
        <Table.Row>
          <Table.ColumnHeaderCell>Number of Retries</Table.ColumnHeaderCell>
          <Table.Cell>{existingConf.num_retries || 0}</Table.Cell>
        </Table.Row>
        <Table.Row>
          <Table.ColumnHeaderCell>Retry Interval (sec)</Table.ColumnHeaderCell>
          <Table.Cell>{existingConf.interval_sec || 10}</Table.Cell>
        </Table.Row>
        <Table.Row>
          <Table.ColumnHeaderCell>Retry Interval (sec)</Table.ColumnHeaderCell>
          <Table.Cell>{existingConf.interval_sec || 10}</Table.Cell>
        </Table.Row>
        <Table.Row>
          <Table.ColumnHeaderCell>Timeout (sec)</Table.ColumnHeaderCell>
          <Table.Cell>{existingConf.timeout_sec || 60}</Table.Cell>
        </Table.Row>
      </Table.Body>
    </Table.Root>
  );

  const expanded = () => (
    <CommonRetryConf
      retryConf={conf}
      setRetryConf={setRetryConf}
      legacyTooltip={false}
    />
  );

  return (
    <div className="my-4 mt-2 w-6/12">
      <Heading size="3">Retry Configuration</Heading>
      <div className="mb-4">
        <Text>Edit your retry setting for event failures.</Text>
      </div>
      <ExpandableEditor
        editorCollapsed={collapsed}
        editorExpanded={expanded}
        property="retry"
        saveFunc={save}
        service="modify-trigger"
        expandCallback={reset}
        dataTest="edit-retry-config"
      />
    </div>
  );
};

export default RetryConfEditor;
