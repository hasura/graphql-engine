import { useViewDefinition } from '@hasura/metadata/data-source';
import { QualifiedDataSource, Table } from '@hasura/shared/types';
import { RawSqlButton, SqlCodeBlock, Text } from '@hasura/shared/ui';
import { Flex, Skeleton } from '@radix-ui/themes';

type Props = {
  source: QualifiedDataSource;
  table: Table;
  readOnly: boolean;
};
const ViewDefinition = ({ source, table, readOnly }: Props) => {
  const { data: viewDef, isFetching } = useViewDefinition({
    source,
    table,
  });

  const renderContent = () => {
    if (!isFetching && !viewDef?.definition) {
      return (
        <Text as="p" className="italic">
          Unable to fetch view definition
        </Text>
      );
    }

    return (
      <Skeleton loading={isFetching}>
        <SqlCodeBlock
          size="1"
          scrollable
          language={source.kind}
          text={viewDef?.definition ?? ''}
        />
      </Skeleton>
    );
  };

  return (
    <div className="w-full mb-4">
      <Flex align="center" gap="2" className="mb-2">
        <Text weight="medium">View Definition:</Text>
        {!readOnly && viewDef?.definition ? (
          <RawSqlButton sql={viewDef.definition} data-test="modify-view">
            Modify
          </RawSqlButton>
        ) : null}
      </Flex>
      {renderContent()}
    </div>
  );
};

export default ViewDefinition;
