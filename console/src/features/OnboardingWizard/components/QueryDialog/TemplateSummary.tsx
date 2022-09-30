import * as React from 'react';
import { useQuery } from 'react-query';
import { tracingTools } from '@/features/TracingTools';
import { Dispatch } from '@/types';
import {
  staleTime,
  templateSummaryRunQueryClickVariables,
  templateSummaryRunQuerySkipVariables,
} from '../../constants';
import { QueryDialog } from './QueryDialog';
import {
  fetchTemplateDataQueryFn,
  getQueryFromSampleQueries,
  runQueryInGraphiQL,
  fillSampleQueryInGraphiQL,
  emitOnboardingEvent,
} from '../../utils';

const defaultQuery = `
# Lookup artist info, albums, tracks based on relations
# Filter for only 'ArtistId' with the ID of '22'

query lookupArtist {
    sample_Artist(where: {ArtistId: {_eq: 22}}) {
            ArtistId
            Name
            Albums {
            AlbumId
            Title
            Tracks {
                TrackId
                Name
            }
        }
    }
}
`;

type Props = {
  templateUrl: string;
  dismiss: VoidFunction;
  dispatch: Dispatch;
};

export function TemplateSummary(props: Props) {
  const { templateUrl, dismiss, dispatch } = props;
  const schemaImagePath = `${templateUrl}/diagram.png`;
  const sampleQueriesPath = `${templateUrl}/sample.graphql`;

  const [sampleQuery, setSampleQuery] = React.useState(defaultQuery);

  const { data: sampleQueriesData } = useQuery({
    queryKey: sampleQueriesPath,
    queryFn: () => fetchTemplateDataQueryFn(sampleQueriesPath, {}),
    staleTime,
    onSuccess: (allQueries: string) => {
      try {
        const gqlQuery = getQueryFromSampleQueries(allQueries, 'lookupArtist');
        setSampleQuery(gqlQuery || defaultQuery);
      } catch (e: any) {
        // this is unexpected; so get alerted
        tracingTools.sentry.captureException(
          new Error('failed to get a sample query in template summary'),
          {
            debug: {
              error: 'message' in e ? e.message : e,
              trace: 'OnboardingWizard/TemplateSummary',
            },
          }
        );
      }
    },
    onError: (e: any) => {
      // this is unexpected; so get alerted
      tracingTools.sentry.captureException(
        new Error('failed to fetch sample queries in template summary'),
        {
          debug: {
            error: 'message' in e ? e.message : e,
            trace: 'OnboardingWizard/TemplateSummary',
          },
        }
      );
    },
  });

  // this effect makes sure that the query is filled in GraphiQL as soon as possible
  React.useEffect(() => {
    if (sampleQueriesData) {
      fillSampleQueryInGraphiQL(sampleQuery, dispatch);
    }
  }, [sampleQueriesData, sampleQuery]);

  // this runs the query that is prefilled in graphiql
  const onRunHandler = () => {
    emitOnboardingEvent(templateSummaryRunQueryClickVariables);
    runQueryInGraphiQL();
    dismiss();
  };
  const onSkipHandler = () => {
    emitOnboardingEvent(templateSummaryRunQuerySkipVariables);
    dismiss();
  };

  return (
    <QueryDialog
      title="Welcome to Hasura"
      description="Get started learning Hasura with an example."
      query={sampleQuery}
      schemaImage={schemaImagePath}
      onRunHandler={onRunHandler}
      onSkipHandler={onSkipHandler}
    />
  );
}
