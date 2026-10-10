import { MetadataSelectors } from '@hasura/metadata/helpers';
import { MetadataWrapper } from '../components';
import { IndicatorCard } from '@hasura/shared/ui';
import { Outlet, useParams } from 'react-router';
import { DataSourceContext } from '../context/DataSourceContext';

export const ManageDatabaseRoute = () => {
  const params = useParams();
  if (!params.source)
    return (
      <div className="p-8">
        <IndicatorCard status="negative" showIcon>
          Source could not be found in Metadata!
        </IndicatorCard>
      </div>
    );

  return (
    <MetadataWrapper
      render={({ data: meta }) => {
        const source = MetadataSelectors.findSource(params.source)(meta);

        // if we don't find the source, report an error:
        if (!source || !meta) {
          return (
            <div className="p-8">
              <IndicatorCard status="negative" showIcon>
                Source {params.source} could not be found in Metadata!
              </IndicatorCard>
            </div>
          );
        }

        return (
          <DataSourceContext.Provider
            value={{
              ...meta,
              currentSource: source,
            }}
          >
            <Outlet />
          </DataSourceContext.Provider>
        );
      }}
    />
  );
};
