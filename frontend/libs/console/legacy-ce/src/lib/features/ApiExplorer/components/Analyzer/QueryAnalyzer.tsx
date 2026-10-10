import { useEffect, useState } from 'react';
import RootFields from './RootFields';
import {
  hasuraToast,
  Dialog,
  PlainCodeBlock,
  SqlCodeBlock,
  Text,
} from '@hasura/shared/ui';
import { analyzeFetcher, ExplainResult } from './utils';
import { GraphQLRequestInput, GraphiqlMode } from '@hasura/shared/types';
import { useErrorNotification } from '@hasura/metadata/api';
import { Flex } from '@radix-ui/themes';

type Props = {
  mode: GraphiqlMode;
  query: GraphQLRequestInput;
  headers: Record<string, string>;
  clearAnalyse: () => void;
};

const QueryAnalyzer = ({ mode, query, headers, clearAnalyse }: Props) => {
  const showErrorNotification = useErrorNotification();
  const [analyseData, setAnalyseData] = useState<ExplainResult[]>([]);
  const [activeNode, setActiveNode] = useState(0);

  useEffect(() => {
    analyzeFetcher(query, headers, mode === 'relay')
      .then((data) => {
        // todo: unsure if this guard is necessary. Replaces previous guard that would silently return
        // this was previously necessary as the analyze fetcher would handle errors without throwing
        const isNotValidData =
          data && data[0]?.plan === null && data[0]?.sql === null;
        if (isNotValidData) {
          console.error(
            'Missing data from analyze result. This should never happen.',
          );

          hasuraToast({
            type: 'error',
            message: 'Missing data from analyze result.',
          });
          return;
        }

        setAnalyseData(data);
        setActiveNode(0);
      })
      .catch((err) => {
        clearAnalyse();
        showErrorNotification({
          title: 'Analyze query error',
          error: err,
        });
      });
  }, []);

  const handleAnalyseNodeChange = (e) => {
    const nodeKey = e.target.getAttribute('data-key');
    if (nodeKey) {
      setActiveNode(parseInt(nodeKey, 10));
    }
  };

  return (
    <Dialog size="max" onClose={clearAnalyse} title="Query Analysis" separator>
      <Flex className="min-h-full">
        <div className="w-1/4">
          <div className="h-8/12">
            <div className="mb-4">
              <Text as="div" color="indigo" weight="bold" size="3">
                Top level nodes
              </Text>
            </div>
            <RootFields
              data={analyseData}
              activeNode={activeNode}
              onClick={handleAnalyseNodeChange}
            />
          </div>
        </div>
        <div className="w-3/4">
          <div className="w-full">
            <div className="p-4 pt-0">
              <Text size="3" as="div" weight="bold">
                Generated Query
              </Text>
              <div className="w-full overflow-y-scroll h-[calc(30vh)] my-2 relative">
                <SqlCodeBlock text={analyseData[activeNode]?.sql ?? ''} />
              </div>
            </div>
            <div className="p-4 pt-0">
              <Text size="3" as="div" weight="bold">
                Execution Plan
              </Text>
              <div className="w-full h-[calc(30vh)] overflow-y-scroll my-2 relative">
                <PlainCodeBlock
                  value={analyseData[activeNode]?.plan?.join('\n') ?? ''}
                />
              </div>
            </div>
          </div>
        </div>
      </Flex>
    </Dialog>
  );
};

export default QueryAnalyzer;
