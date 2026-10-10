import { Card, Flex, Heading } from '@radix-ui/themes';
import { Badge, Collapsible, Separator, Text } from '@hasura/shared/ui';

export type SourceLevelSummary = {
  dataSourceName: string;
  totalCount: number;
};

export type Props = {
  tablesAndViews?: SourceLevelSummary[];
  collections?: SourceLevelSummary[];
  logicalModels: SourceLevelSummary[];
  isOssMode?: boolean;
};

const calculateTotal = (items: SourceLevelSummary[]) => {
  return items.reduce((acc, item) => acc + item.totalCount, 0);
};

const SummaryView = ({
  label,
  items,
}: {
  label: string;
  items: SourceLevelSummary[];
}) => {
  return (
    <div>
      <Collapsible
        triggerClassName="w-full"
        triggerChildren={
          <Flex justify="between" align="center" className="p-4">
            <Text weight="medium">{label}</Text>
            <Flex align="center" className="gap-1.5">
              <Badge color="gray">
                <Text size="2">{calculateTotal(items)}</Text>
              </Badge>
            </Flex>
          </Flex>
        }
      >
        {items.map((source, i) => {
          return (
            <>
              <Flex
                key={source.dataSourceName}
                justify="between"
                align="center"
                className="pl-8"
              >
                <Text>{source.dataSourceName}</Text>
                <Text>{source.totalCount}</Text>
              </Flex>
              {i < items.length && <Separator className="my-4" size="4" />}
            </>
          );
        })}
      </Collapsible>
    </div>
  );
};

export const ModelSummary = ({
  tablesAndViews = [],
  collections = [],
  logicalModels = [],
  isOssMode = false,
}: Props) => {
  return (
    <div className="my-4">
      <div className="py-4 px-8">
        <Heading size="6">Model Count Summary</Heading>

        <Text as="p">
          The summary of all your tables, views, collections and logical models
          tracked in your metadata
        </Text>
      </div>

      <Separator size="4" className="my-4" />

      <Card className="mx-8">
        <Flex justify="between" align="center" className="p-4">
          <Text size="4">Total Number of Models</Text>
          <Badge color="indigo">
            <Text size="4">
              {calculateTotal(tablesAndViews) +
                calculateTotal(collections) +
                calculateTotal(logicalModels)}
            </Text>
          </Badge>
        </Flex>

        <Separator size="4" className="my-1.5" />

        <SummaryView label={'Tables and Views'} items={tablesAndViews} />

        {!isOssMode && (
          <SummaryView label={'Collections'} items={collections} />
        )}

        <SummaryView label={'Logical Models'} items={logicalModels} />
      </Card>
    </div>
  );
};
