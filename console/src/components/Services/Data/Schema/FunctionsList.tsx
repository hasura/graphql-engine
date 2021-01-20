import React, { useReducer } from 'react';

import { isEmpty } from '../../../Common/utils/jsUtils';
import { Dispatch } from '../../../../types';
import { addExistingFunction } from '../Add/AddExistingTableViewActions';
import Button from '../../../Common/Button';
import RawSqlButton from '../Common/Components/RawSqlButton';
import { Note } from '../../../Common/Note';
import styles from './styles.scss';
import { PGFunction } from '../../../../dataSources/services/postgresql/types';

type VolatileFuncNoteProps = {
  showQueryNote: boolean;
  onCancelClick: () => void;
  onTrackAsMutationClick: () => void;
  onTrackAsQueryClick: () => void;
  onTrackAsQueryConfirmClick: () => void;
};
const VolatileFuncNote: React.FC<VolatileFuncNoteProps> = ({
  showQueryNote,
  onCancelClick,
  onTrackAsMutationClick,
  onTrackAsQueryClick,
  onTrackAsQueryConfirmClick,
}) => {
  return (
    <Note type="warn">
      This function will be tracked as a <b>Mutation</b> because the function is{' '}
      <b>VOLATILE</b>
      <div className={styles.buttonsSection}>
        <Button
          color="yellow"
          onClick={onTrackAsMutationClick}
          data-test="track-as-mutation"
        >
          Track as Mutation
        </Button>
        <Button onClick={onCancelClick}>Cancel</Button>
        <button
          className={`${styles.btnBlank} ${styles.queryButton}`}
          onClick={onTrackAsQueryClick}
          data-test="track-as-query"
        >
          Track as Query
        </button>
      </div>
      {showQueryNote ? (
        <div className={styles.nestedBox}>
          <b>Are you sure?</b> Queries are supposed to be read-only and as such
          are recommended to be <b>STABLE</b> or <b>IMMUTABLE</b>.
          <div className={styles.buttonsSection}>
            <Button
              onClick={onTrackAsQueryConfirmClick}
              data-test="track-as-query-confirm"
            >
              Track as Query
            </Button>
          </div>
        </div>
      ) : null}
    </Note>
  );
};

type State = {
  volatileNoteOpen: boolean;
  queryWarningOpen: boolean;
};

const defaultState: State = {
  volatileNoteOpen: false,
  queryWarningOpen: false,
};

type Action =
  | 'click-track-volatile-func'
  | 'click-track-volatile-func-as-query'
  | 'track-volatile-func'
  | 'cancel';

const reducer = (state: State, action: Action): State => {
  switch (action) {
    case 'click-track-volatile-func':
      return {
        ...state,
        volatileNoteOpen: true,
      };
    case 'click-track-volatile-func-as-query':
      return {
        ...state,
        volatileNoteOpen: true,
        queryWarningOpen: true,
      };

    case 'track-volatile-func':
    case 'cancel':
      return defaultState;

    default:
      return state;
  }
};

type TrackableEntryProps = {
  readOnlyMode: boolean;
  dispatch: Dispatch;
  func: PGFunction;
  index: number;
  source: string;
};
const TrackableEntry: React.FC<TrackableEntryProps> = ({
  readOnlyMode,
  dispatch,
  func,
  index,
  source,
}) => {
  const [state, dispatchR] = useReducer(reducer, defaultState);
  const isVolatile = func.function_type.toLowerCase() === 'volatile';

  const handleTrackFn = (e: React.MouseEvent) => {
    e.preventDefault();
    if (isVolatile) {
      dispatchR('click-track-volatile-func');
    } else {
      dispatch(addExistingFunction(func.function_name));
    }
  };

  const handleTrackFunction = (type: 'query' | 'mutation') => {
    dispatchR('track-volatile-func');
    dispatch(
      addExistingFunction(func.function_name, {
        exposed_as: type,
      })
    );
  };

  return (
    <div className={styles.padd_bottom} key={`untracked-function-${index}`}>
      {!readOnlyMode ? (
        <div className={styles.display_inline}>
          <Button
            data-test={`add-track-function-${func.function_name}`}
            className={`${styles.display_inline} btn btn-xs btn-default`}
            onClick={handleTrackFn}
          >
            Track
          </Button>
        </div>
      ) : null}
      <div className={`${styles.display_inline} ${styles.add_mar_left_mid}`}>
        <RawSqlButton
          dataTestId={`view-function-${func.function_name}`}
          customStyles={styles.display_inline}
          sql={func.function_definition}
          dispatch={dispatch}
          source={source}
        >
          View
        </RawSqlButton>
      </div>
      <div className={`${styles.display_inline} ${styles.add_mar_left}`}>
        <span>{func.function_name}</span>
      </div>

      {isVolatile && state.volatileNoteOpen && (
        <div className={styles.volatileNote}>
          <VolatileFuncNote
            showQueryNote={state.queryWarningOpen}
            onCancelClick={() => dispatchR('cancel')}
            onTrackAsMutationClick={() => handleTrackFunction('mutation')}
            onTrackAsQueryClick={() =>
              dispatchR('click-track-volatile-func-as-query')
            }
            onTrackAsQueryConfirmClick={() => handleTrackFunction('query')}
          />
        </div>
      )}
    </div>
  );
};

type FunctionsListProps = {
  funcs: PGFunction[];
  readOnlyMode: boolean;
  dispatch: Dispatch;
  source: string;
};
export const TrackableFunctionsList: React.FC<FunctionsListProps> = ({
  funcs,
  ...props
}) => {
  const noTrackableFunctions = isEmpty(funcs);

  if (noTrackableFunctions) {
    return (
      <div key="no-untracked-fns">
        <div>There are no untracked functions</div>
      </div>
    );
  }

  return (
    <>
      {funcs.map((p, i) => (
        <TrackableEntry key={p.function_name} {...props} func={p} index={i} />
      ))}
    </>
  );
};
