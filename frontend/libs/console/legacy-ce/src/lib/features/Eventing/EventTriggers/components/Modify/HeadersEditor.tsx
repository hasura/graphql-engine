import {
  CardedTable,
  ExpandableEditor,
  ExpandableEditorFunction,
  HeadersInput,
  parseServerHeaders,
  Text,
} from '@hasura/shared/ui';
import { ClientHeader, EventTrigger } from '@hasura/shared/types';
import { Em, Heading, Strong } from '@radix-ui/themes';

type HeaderEditorProps = {
  currentTrigger: EventTrigger;
  headers: ClientHeader[];
  setHeaders: (h: ClientHeader[]) => void;
  save: ExpandableEditorFunction;
};

const HeadersEditor = ({
  setHeaders,
  headers,
  save,
  currentTrigger,
}: HeaderEditorProps) => {
  const existingHeaders = parseServerHeaders(currentTrigger.headers);
  const numExistingHeaders = currentTrigger.headers
    ? currentTrigger.headers.length
    : 0;

  const reset = () => {
    setHeaders(existingHeaders);
  };

  const collapsed = () => (
    <>
      {numExistingHeaders > 0 ? (
        <CardedTable
          columns={['Key', 'Type', 'Value']}
          data={existingHeaders
            .filter((h) => !!h.name)
            .map((header) => [
              <Strong key={header.name}>{header.name}</Strong>,
              header.type,
              header.value,
            ])}
        />
      ) : (
        <Text>
          <Em>No headers</Em>
        </Text>
      )}
    </>
  );

  const expanded = () => (
    <div>
      <HeadersInput headers={headers} setHeaders={setHeaders} />
    </div>
  );

  return (
    <div className="w-6/12">
      <Heading size="3">Headers</Heading>
      <Text as="p" size="1">
        Headers Hasura will send to the webhook with the POST request.
      </Text>
      <ExpandableEditor
        editorCollapsed={collapsed}
        editorExpanded={expanded}
        expandCallback={reset}
        property="headers"
        service="modify-trigger"
        saveFunc={save}
        dataTest="edit-header"
      />
    </div>
  );
};

export default HeadersEditor;
