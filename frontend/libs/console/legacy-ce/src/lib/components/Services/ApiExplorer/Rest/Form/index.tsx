import React from 'react';
import { RouteComponentProps } from 'react-router';
import { connect, ConnectedProps } from 'react-redux';
import queryString from 'query-string';
import {
  Analytics,
  REDACT_EVERYTHING,
} from '../../../../../features/Analytics';
import { parse, print } from 'graphql';
import {
  AllowedRESTMethods,
  RestEndpointEntry,
} from '../../../../../metadata/types';
import { useIsUnmounted } from '../../../Data/Common/tsUtils';
import { Dispatch, ReduxState } from '../../../../../types';
import {
  addRESTEndpoint,
  editRESTEndpoint,
} from '../../../../../metadata/actions';
import { allowedQueriesCollection } from '../../../../../metadata/utils';
import { showErrorNotification } from '../../../Common/Notification';
import {
  RestEndpointForm,
  RestEndpointFormState,
  RestEndpointFormData,
} from './RestEndpointForm';
import { getLSItem, LS_KEYS } from '../../../../../utils/localStorage';
import _push from '../../../Data/push';

const forgeFormEndpointObject = (
  state: RestEndpointFormData
): [RestEndpointEntry, string] => [
  {
    name: state?.name,
    url: state?.url,
    definition: {
      query: {
        query_name: state?.name,
        collection_name: allowedQueriesCollection,
      },
    },
    methods: state?.methods,
    comment: state.comment,
  },
  state.request,
];

interface FormEndpointProps extends InjectedProps {
  location: RouteComponentProps<unknown, unknown>['location'];
  routeParams: RouteComponentProps<unknown, { name: string }>['routeParams'];
}

type RestEndpointFormSubmitHandler = (
  restEndpoint: RestEndpointEntry,
  request: string,
  cb: () => void,
  currentRestEndpoint?: RestEndpointEntry
) => void;

type RestEndpointCreateAction = (
  restEndpoint: RestEndpointEntry,
  request: string,
  cb: () => void
) => void;

type RestEndpointEditAction = (
  restEndpoint: RestEndpointEntry,
  request: string,
  cb: () => void,
  currentRestEndpoint: RestEndpointEntry
) => void;

type RestEndpointFormStateHook = (
  createEndpoint: RestEndpointCreateAction,
  editEndpoint?: RestEndpointEditAction,
  props?: Pick<FormEndpointProps, 'routeParams' | 'metadataObject'>
) => {
  formState: RestEndpointFormState;
  formSubmitHandler: RestEndpointFormSubmitHandler;
};

const useRestEndpointFormStateForCreation: RestEndpointFormStateHook = (
  createEndpoint
): {
  formState: RestEndpointFormState;
  formSubmitHandler: RestEndpointFormSubmitHandler;
} => {
  const formState: RestEndpointFormState = {};
  try {
    const parsedQuery = queryString.parseUrl(window.location.href);
    if (parsedQuery.query?.from === 'graphiql') {
      formState.request = print(parse(getLSItem(LS_KEYS.graphiqlQuery) ?? ''));
    }
  } catch (e) {
    // ignore
  }
  return { formState, formSubmitHandler: createEndpoint };
};

const useRestEndpointFormStateForEdition: RestEndpointFormStateHook = (
  createEndpoint,
  editEndpoint,
  props
) => {
  const formState: RestEndpointFormState = {};
  let formSubmitHandler: (
    restEndpoint: RestEndpointEntry,
    request: string,
    cb: () => void
  ) => void;
  const currentPageName = props?.routeParams.name;
  const currentEndpoints = props?.metadataObject?.rest_endpoints ?? [];
  const currentCollections = props?.metadataObject?.query_collections;
  const currentRestEndpointEntry =
    currentEndpoints.find(et => et.name === currentPageName) ?? null;
  const currentEndpointQuery = currentCollections
    ?.find(qce => qce.name === allowedQueriesCollection)
    ?.definition?.queries?.find(qc => qc.name === currentPageName);

  if (currentRestEndpointEntry && editEndpoint) {
    formState.name = currentRestEndpointEntry?.name;
    formState.comment = currentRestEndpointEntry?.comment;
    formState.url = currentRestEndpointEntry?.url;
    formState.methods = currentRestEndpointEntry?.methods;
    formState.request = currentEndpointQuery?.query;

    formSubmitHandler = (restEndpoint, request, cb) =>
      editEndpoint(restEndpoint, request, cb, currentRestEndpointEntry);
  } else {
    console.error('No current endpoint found');
    formSubmitHandler = createEndpoint;
  }

  return { formState, formSubmitHandler };
};

// This component will be used for the purposes of edit as well
const FormEndpoint: React.FC<FormEndpointProps> = ({
  routeToPage,
  createEndpoint,
  location,
  metadataObject,
  editEndpoint,
  routeParams,
}) => {
  const isUnMounted = useIsUnmounted();
  const [loading, setLoading] = React.useState(false);
  const mode = location.pathname === '/api/rest/create' ? 'create' : 'edit';

  const {
    formState,
    formSubmitHandler,
  }: {
    formState: RestEndpointFormState;
    formSubmitHandler: RestEndpointFormSubmitHandler;
  } = {
    create: useRestEndpointFormStateForCreation,
    edit: useRestEndpointFormStateForEdition,
  }[mode](createEndpoint, editEndpoint, { metadataObject, routeParams });

  const resetPageState = () => {
    routeToPage('/api/rest/list');
  };

  const onSubmit = async (data: Record<string, unknown>) => {
    setLoading(true);
    const state: RestEndpointFormData = {
      name: (data.name as string).trim(),
      // null is respected considering the old Hasura versions <2.10
      comment:
        (data.comment as string) === null
          ? ''
          : (data.comment as string).trim(),
      url: (data.url as string).trim(),
      methods: data.methods as AllowedRESTMethods[],
      request: (data.request as string).trim(),
    };
    const [restEndpointObj, request] = forgeFormEndpointObject(state);

    await formSubmitHandler(restEndpointObj, request, resetPageState);
    if (!isUnMounted()) {
      setLoading(false);
    }
  };
  const onCancelHandler = resetPageState;

  return (
    <Analytics
      name={mode === 'create' ? 'FormRestCreate' : 'FormRestEdit'}
      {...REDACT_EVERYTHING}
    >
      <RestEndpointForm
        mode={mode}
        formState={formState}
        loading={loading}
        onSubmit={onSubmit}
        onCancel={onCancelHandler}
      />
    </Analytics>
  );
};

const mapDispatchToProps = (dispatch: Dispatch) => ({
  routeToPage: (link: string) => dispatch(_push(link)),
  createEndpoint: (
    restEndpoint: RestEndpointEntry,
    request: string,
    cb: () => void
  ) => dispatch(addRESTEndpoint(restEndpoint, request, cb)),
  editEndpoint: (
    newEntry: RestEndpointEntry,
    request: string,
    cb: () => void,
    oldEntry: RestEndpointEntry
  ) => dispatch(editRESTEndpoint(oldEntry, newEntry, request, cb)),
  showError: (title: string, message?: string) =>
    dispatch(showErrorNotification(title, message)),
});
const mapStateToProps = (state: ReduxState) => ({
  metadataObject: state.metadata.metadataObject,
});
const createEndpointConnector = connect(mapStateToProps, mapDispatchToProps);
type InjectedProps = ConnectedProps<typeof createEndpointConnector>;

const ConnectedComponent = createEndpointConnector(FormEndpoint);

export default ConnectedComponent;
