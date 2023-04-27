import Skeleton from 'react-loading-skeleton';
import { useQuery } from 'react-query';
import { Button } from '../../../../new-components/Button';
import { CardedTable } from '../../../../new-components/CardedTable';
import { DropdownMenu } from '../../../../new-components/DropdownMenu';
import { DataSource, Feature, IntrospectedFunction } from '../../../DataSource';
import {
  areTablesEqual,
  MetadataSelectors,
  useInvalidateMetadata,
  useMetadata,
} from '../../../hasura-metadata-api';
import { useHttpClient } from '../../../Network';
import { useTrackFunction } from '../hooks/useTrackFunction';
import { adaptFunctionName, search } from '../utils';
import { FunctionDisplayName } from './FunctionDisplayName';
import { SlOptionsVertical } from 'react-icons/sl';
import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';
import { SearchBar } from '../../TrackResources/components/SearchBar';
import { Badge } from '../../../../new-components/Badge';
import { useState } from 'react';
import {
  DEFAULT_PAGE_NUMBER,
  DEFAULT_PAGE_SIZE,
  DEFAULT_PAGE_SIZES,
} from '../../TrackResources/constants';
import { FaAngleLeft, FaAngleRight } from 'react-icons/fa';
import { paginate } from '../../TrackResources/utils';
import { getDefaultQueryOptions } from '../../reactQueryUtils';

type UntrackedFunctionsProps = {
  dataSourceName: string;
};

const useGetUntrackedFunctions = (
  dataSourceName: string,
  autoFireOnMount = true
) => {
  const httpClient = useHttpClient();

  const { data: trackedFunctions = [], isFetching } = useMetadata(m =>
    (MetadataSelectors.findSource(dataSourceName)(m)?.functions ?? []).map(fn =>
      adaptFunctionName(fn.function)
    )
  );

  return useQuery({
    queryKey: [dataSourceName, 'functions'],
    queryFn: async () => {
      const result = await DataSource(httpClient).getTrackableFunctions(
        dataSourceName
      );

      if (result === Feature.NotImplemented) return result;

      return (result ?? []).filter(fn => {
        const isAlreadyTracked = trackedFunctions.find(trackedFn =>
          areTablesEqual(fn.qualifiedFunction, trackedFn)
        );
        return !isAlreadyTracked;
      });
    },
    ...getDefaultQueryOptions<Feature | IntrospectedFunction[]>(),
    enabled: autoFireOnMount && !isFetching,
  });
};

export const UntrackedFunctions = (props: UntrackedFunctionsProps) => {
  const { dataSourceName } = props;

  const [pageNumber, setPageNumber] = useState(DEFAULT_PAGE_NUMBER);
  const [pageSize, setPageSize] = useState(DEFAULT_PAGE_SIZE);
  const [searchText, setSearchText] = useState('');

  const { data: untrackedFunctions = [], isLoading } =
    useGetUntrackedFunctions(dataSourceName);

  const invalidateMetadata = useInvalidateMetadata();

  const { trackFunction } = useTrackFunction({
    dataSourceName,
  });

  if (isLoading) return <Skeleton count={5} height={20} className="mb-1" />;

  if (untrackedFunctions === Feature.NotImplemented) return null;

  if (!untrackedFunctions.length)
    return (
      <IndicatorCard status="info" headline="No untracked functions found">
        We couldn't find any compatible functions in your database that can be
        tracked in Hasura.{' '}
        <LearnMoreLink href="https://hasura.io/docs/latest/schema/postgres/postgres-guides/functions/" />
      </IndicatorCard>
    );

  const filteredResult = search(untrackedFunctions, searchText);

  return (
    <div>
      <div className="flex justify-between space-x-4 mb-sm">
        <div className="flex gap-5">
          <div className="flex gap-2">
            <SearchBar
              onSearch={data => {
                setSearchText(data);
                setPageNumber(DEFAULT_PAGE_NUMBER);
              }}
            />
            {searchText.length ? (
              <Badge>{filteredResult.length} results found</Badge>
            ) : null}
          </div>
        </div>

        <div className="flex gap-1">
          <Button
            icon={<FaAngleLeft />}
            onClick={() => setPageNumber(pageNumber - 1)}
            disabled={pageNumber === 1}
          />
          <select
            value={pageSize}
            onChange={e => {
              setPageSize(Number(e.target.value));
            }}
            className="block w-full max-w-xl h-8 min-h-full shadow-sm rounded pl-3 pr-6 py-0.5 border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400"
          >
            {DEFAULT_PAGE_SIZES.map(_pageSize => (
              <option key={_pageSize} value={_pageSize}>
                Show {_pageSize} tables
              </option>
            ))}
          </select>
          <Button
            icon={<FaAngleRight />}
            onClick={() => setPageNumber(pageNumber + 1)}
            disabled={pageNumber >= filteredResult.length / pageSize}
          />
        </div>
      </div>
      <CardedTable.Table>
        <CardedTable.TableHead>
          <CardedTable.TableHeadRow>
            <CardedTable.TableHeadCell>Function</CardedTable.TableHeadCell>
            <CardedTable.TableHeadCell>
              <div className="float-right">
                <DropdownMenu
                  items={[
                    [
                      <span
                        className="py-2"
                        onClick={() => invalidateMetadata()}
                      >
                        Refresh
                      </span>,
                    ],
                  ]}
                  options={{
                    content: {
                      alignOffset: -50,
                      avoidCollisions: false,
                    },
                  }}
                >
                  <SlOptionsVertical />
                </DropdownMenu>
              </div>
            </CardedTable.TableHeadCell>
          </CardedTable.TableHeadRow>
        </CardedTable.TableHead>
        <CardedTable.TableBody>
          {paginate(filteredResult, pageSize, pageNumber).map(
            untrackedFunction => (
              <CardedTable.TableBodyRow>
                <CardedTable.TableBodyCell>
                  <FunctionDisplayName
                    qualifiedFunction={untrackedFunction.qualifiedFunction}
                  />
                </CardedTable.TableBodyCell>
                <CardedTable.TableBodyCell>
                  <div className="flex gap-2 justify-end">
                    {untrackedFunction.isVolatile ? (
                      <>
                        <Button
                          onClick={() =>
                            trackFunction({
                              function: untrackedFunction.qualifiedFunction,
                              configuration: {
                                exposed_as: 'mutation',
                              },
                              source: dataSourceName,
                            })
                          }
                        >
                          Track as Mutation
                        </Button>
                        <Button
                          onClick={() =>
                            trackFunction({
                              function: untrackedFunction.qualifiedFunction,
                              configuration: {
                                exposed_as: 'query',
                              },
                              source: dataSourceName,
                            })
                          }
                        >
                          Track as Query
                        </Button>
                      </>
                    ) : (
                      <Button
                        onClick={() => {
                          trackFunction({
                            function: untrackedFunction.qualifiedFunction,
                            source: dataSourceName,
                          });
                        }}
                      >
                        Track as Root Field
                      </Button>
                    )}
                  </div>
                </CardedTable.TableBodyCell>
              </CardedTable.TableBodyRow>
            )
          )}
        </CardedTable.TableBody>
      </CardedTable.Table>
    </div>
  );
};
