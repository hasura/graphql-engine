import * as React from 'react';
import { useQuery } from 'react-query';
import { programmaticallyTraceError } from '../../../../Analytics';
import { Dispatch } from '../../../../../types';
import {
  staleTime,
  templateSummaryRunQueryClickVariables,
  templateSummaryRunQuerySkipVariables,
} from '../../../constants';
import { QueryScreen } from '../../../components/QueryScreen/QueryScreen';
import { fetchTemplateDataQueryFn } from '../../utils';
import {
  runQueryInGraphiQL,
  fillSampleQueryInGraphiQL,
  emitOnboardingEvent,
} from '../../../utils';

type Props = {
  templateUrl: string;
  dismiss: VoidFunction;
  dispatch: Dispatch;
};

const defaultQuery = `
#
#  An example query:
#  Lookup all customers and their orders based on a foreign key relationship.
#  ┌──────────┐     ┌───────┐
#  │ customer │---->│ order │
#  └──────────┘     └───────┘
#

query lookupCustomerOrder {
  customer {
    id
    first_name
    last_name
    username
    email
    phone
    orders {
      id
      order_date
      product
      purchase_price
      discount_price
    }
  }
}
`;

export function TemplateSummary(props: Props) {
  const { templateUrl, dismiss, dispatch } = props;
  const schemaImagePath = `${templateUrl}/diagram.png`;
  const sampleQueriesPath = `${templateUrl}/sample.graphql`;

  const [sampleQuery, setSampleQuery] = React.useState(defaultQuery);

  const { data: sampleQueriesData } = useQuery({
    queryKey: sampleQueriesPath,
    queryFn: () => fetchTemplateDataQueryFn(sampleQueriesPath, {}),
    staleTime,
    onError: (e: any) => {
      // this is unexpected; so get alerted
      programmaticallyTraceError({
        error: 'failed to get a sample query in template summary',
        cause: e,
      });
    },
  });

  // this effect makes sure that the query is filled in GraphiQL as soon as possible
  React.useEffect(() => {
    if (typeof sampleQueriesData === 'string') {
      setSampleQuery(sampleQueriesData);
      fillSampleQueryInGraphiQL(sampleQueriesData, dispatch);
    }
  }, [sampleQueriesData]);

  // this runs the query that is prefilled in graphiql
  const onRunHandler = () => {
    emitOnboardingEvent(templateSummaryRunQueryClickVariables);
    if (sampleQueriesData) {
      runQueryInGraphiQL();
    }
    dismiss();
  };
  const onSkipHandler = () => {
    emitOnboardingEvent(templateSummaryRunQuerySkipVariables);
    dismiss();
  };

  return (
    <QueryScreen
      schemaImage={schemaImagePath}
      onRunHandler={onRunHandler}
      onSkipHandler={onSkipHandler}
      query={sampleQuery}
    />
  );
}
