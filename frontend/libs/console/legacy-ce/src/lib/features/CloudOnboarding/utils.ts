import {
  clickRunQueryButton,
  forceGraphiQLIntrospection,
  forceChangeGraphiqlQuery,
} from '../../components/Services/ApiExplorer/OneGraphExplorer/utils';
import { Dispatch } from '@/types';

export const runQueryInGraphiQL = () => {
  clickRunQueryButton();
};

export const fillSampleQueryInGraphiQL = (
  query: string,
  dispatch: Dispatch
) => {
  forceGraphiQLIntrospection(dispatch);

  // this timeout makes sure that there's a delay in setting query after introspection has been fired
  // this timeout does not intend to wait for introspection to finish
  setTimeout(() => {
    forceChangeGraphiqlQuery(query, dispatch);
  }, 500);
};
