import * as React from 'react';
import { useQuery } from 'react-query';
import { tracingTools } from '@/features/TracingTools';
import {
  NEON_ONBOARDING_QUERY_KEY,
  staleTime,
  templateSummaryRunQueryClickVariables,
  templateSummaryRunQuerySkipVariables,
} from '../../constants';
import { QueryDialog } from './QueryDialog';
import {
  fetchTemplateDataQueryFn,
  getQueryFromSampleQueries,
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

// TODO use an actual function
const runSampleQueryInGraphiQL = (query: string) => {
  return Promise.resolve(query);
};

type Props = {
  templateUrl: string;
  dismiss: VoidFunction;
};

export function TemplateSummary(props: Props) {
  const { templateUrl, dismiss } = props;
  const schemaImagePath = `${templateUrl}/diagram.png`;
  const sampleQueriesPath = `${templateUrl}/sample.graphql`;

  const [sampleQuery, setSampleQuery] = React.useState(defaultQuery);

  useQuery({
    queryKey: [NEON_ONBOARDING_QUERY_KEY, sampleQueriesPath],
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

  const onRunHandler = () => {
    emitOnboardingEvent(templateSummaryRunQueryClickVariables);
    runSampleQueryInGraphiQL(sampleQuery).then(() => {
      dismiss();
    });
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
