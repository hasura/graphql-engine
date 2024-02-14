import { FaArrowRight } from 'react-icons/fa';
import { RsToRsSchema } from '../../types';

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
    <div className="flex items-center">
      {paths.map((path, i) => (
        <>
          <div key={path} className="text-gray-600 font-semibold">
            {path}
          </div>
          {i !== paths.length - 1 && <FaArrowRight className="mx-2" />}
        </>
      ))}
    </div>
  );
};
