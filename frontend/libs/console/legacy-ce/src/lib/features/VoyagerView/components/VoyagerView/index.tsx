import { Voyager } from 'graphql-voyager';
import 'graphql-voyager/dist/voyager.css';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import './voyagerView.css';
import { useIntrospectSchema } from '@hasura/metadata/api';
import { Skeleton } from '@radix-ui/themes';

const VoyagerView = () => {
  const { data: introspectionProvider, isFetching } = useIntrospectSchema();

  return (
    <Analytics name="VoyagerView" {...REDACT_EVERYTHING}>
      <Skeleton loading={isFetching}>
        <Voyager introspection={introspectionProvider} />
      </Skeleton>
    </Analytics>
  );
};

export default VoyagerView;
