import { FaArrowRight } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import { Text } from '@hasura/shared/ui';
import { RsToRsSchema } from '../RemoteSchemaToRemoteSchemaForm/schemas';

function extractPaths(resultSet: RsToRsSchema['resultSet']): string[] {
  try {
    let results: string[] = [];
    for (const key of Object.keys(resultSet)) {
      if (Object.keys(resultSet[key].arguments ?? {})?.length > 0) {
        const args = resultSet[key].arguments;
        const argList = Object.entries(args)
          .map(([argK, argV]) => `${argK}:${argV}`)
          .join(',');
        results.push(`${key}(${argList})`);
      } else {
        results.push(key);
      }
      results = results.concat(extractPaths(resultSet[key].field || {}));
    }

    return results;
  } catch (e) {
    return [];
  }
}

interface RelationshipOverviewProps {
  resultSet: RsToRsSchema['resultSet'];
}

export const RelationshipOverview = (props: RelationshipOverviewProps) => {
  const { resultSet } = props;
  const paths = extractPaths(resultSet ?? {});
  return (
    <Flex align="center">
      {paths.map((path, i) => (
        <>
          <Text key={path} weight="bold">
            {path}
          </Text>
          {i !== paths.length - 1 && <FaArrowRight className="mx-2" />}
        </>
      ))}
    </Flex>
  );
};
