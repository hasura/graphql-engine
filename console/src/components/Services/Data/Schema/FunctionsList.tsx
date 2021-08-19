import React, { useReducer } from 'react';

import { isEmpty } from '../../../Common/utils/jsUtils';
import { Dispatch, ReduxState } from '../../../../types';
import { addExistingFunction } from '../Add/AddExistingTableViewActions';
import Button from '../../../Common/Button';
import RawSqlButton from '../Common/Components/RawSqlButton';
import { Note } from '../../../Common/Note';
import styles from './styles.scss';
import { PGFunction } from '../../../../dataSources/services/postgresql/types';
import { isTrackableAndComputedField } from './utils';
import _push from '../push';
import { Table } from '../../../../dataSources/types';

type FuncNoteProps = {
  showTrackVolatileNote?: boolean;
  onCancelClick: () => void;
  onClickMainAction: () => void;
  onClickSecondaryAction: () => void;
  showQueryNote?: boolean;
  onTrackAsQueryConfirmClick?: () => void;
};
const FuncNote: React.FC<FuncNoteProps> = ({
  showQueryNote = false,
  showTrackVolatileNote = false,
  onCancelClick,
  onClickMainAction,
  onClickSecondaryAction,
  onTrackAsQueryConfirmClick,
}) => {
  const mainButtonText = showTrackVolatileNote
    ? 'Track As Mutation'
    : 'Add As Root Field';
  const secondaryActionText = showTrackVolatileNote
    ? 'Track As Query'
    : 'Cancel';
  const thirdActionText = showTrackVolatileNote
    ? 'Cancel'
    : 'Add As Computed Field';
  return (
    <Note type="warn">
      {showTrackVolatileNote ? (
        <>
          This function will be tracked as a <b>Mutation</b> because the
          function is <b>VOLATILE</b>
        </>
      ) : (
        <>
          This function can be added as a <b>root field</b> or a{' '}
          <b>computed field</b> inside a table.
        </>
      )}
      <div className={styles.buttonsSection}>
        <Button
          color="yellow"
          onClick={onClickMainAction}
          data-test="track-as-mutation"
        >
          {mainButtonText}
        </Button>
        <Button
          onClick={
            showTrackVolatileNote ? onCancelClick : onClickSecondaryAction
          }
        >
          {thirdActionText}
        </Button>
        <button
          className={`${styles.btnBlank} ${styles.queryButton}`}
          onClick={
            showTrackVolatileNote ? onClickSecondaryAction : onCancelClick
          }
          data-test="track-as-query"
        >
          {secondaryActionText}
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
  stableImmutableNoteOpen: boolean;
};

const defaultState: State = {
  volatileNoteOpen: false,
  queryWarningOpen: false,
  stableImmutableNoteOpen: false,
};

type Action =
  | 'click-track-volatile-func'
  | 'click-track-volatile-func-as-query'
  | 'track-volatile-func'
  | 'cancel'
  | 'click-track-stable-immutable-func';

const reducer = (state: State, action: Action): State => {
  switch (action) {
    case 'click-track-volatile-func':
      return {
        ...state,
        volatileNoteOpen: true,
      };
    case 'click-track-stable-immutable-func':
      return {
        ...state,
        stableImmutableNoteOpen: true,
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
  allSchemas: Table[];
};
const TrackableEntry: React.FC<TrackableEntryProps> = ({
  readOnlyMode,
  dispatch,
  func,
  index,
  source,
  allSchemas,
}) => {
  const [state, dispatchR] = useReducer(reducer, defaultState);
  const isVolatile = func.function_type.toLowerCase() === 'volatile';
  const isTrackableAndUseInComputedField = isTrackableAndComputedField(func);

  const handleTrackFn = (e: React.MouseEvent) => {
    e.preventDefault();
    if (isVolatile) {
      dispatchR('click-track-volatile-func');
    } else if (isTrackableAndUseInComputedField) {
      dispatchR('click-track-stable-immutable-func');
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

      {isTrackableAndUseInComputedField && state.stableImmutableNoteOpen && (
        <div className={styles.volatileNote}>
          <FuncNote
            onCancelClick={() => dispatchR('cancel')}
            onClickMainAction={() =>
              dispatch(addExistingFunction(func.function_name))
            }
            onClickSecondaryAction={() => {
              const currentFuncInfo = func.input_arg_types?.find(
                fn => fn.type === 'c'
              );
              if (!currentFuncInfo) {
                return;
              }
              if (
                !allSchemas?.find(
                  schemaInfo =>
                    schemaInfo?.table_name === currentFuncInfo.name &&
                    schemaInfo?.table_schema === currentFuncInfo.schema
                )
              ) {
                return;
              }
              dispatch(
                _push(
                  `/data/${source}/schema/${currentFuncInfo.schema}/tables/${currentFuncInfo.name}/modify`
                )
              );
            }}
          />
        </div>
      )}

      {isVolatile && state.volatileNoteOpen && (
        <div className={styles.volatileNote}>
          <FuncNote
            showTrackVolatileNote
            showQueryNote={state.queryWarningOpen}
            onCancelClick={() => dispatchR('cancel')}
            onClickMainAction={() => handleTrackFunction('mutation')}
            onClickSecondaryAction={() =>
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
  allSchemas: ReduxState['tables']['allSchemas'];
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
