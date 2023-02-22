import { QualifiedTable } from '../../../../../metadata/types';

export const REL_SET_NAME = 'Relationships/REL_SET_NAME';
export const REL_SET_TYPE = 'Relationships/REL_SET_TYPE';
export const REL_SET_SOURCE = 'Relationships/REL_SET_SOURCE';
export const REL_SET_TABLE = 'Relationships/REL_SET_TABLE';
export const REL_SET_COLUMNS = 'Relationships/REL_SET_COLUMNS';
export const REL_SET_STATE = 'Relationships/REL_SET_STATE';
export const REL_RESET_STATE = 'Relationships/REL_RESET_STATE';
export const REL_SET_DRIVER = 'Relationships/REL_SET_DRIVER';

type DbToDbRelState = {
  relName: string;
  relType: string;
  relSource: string;
  relTable: QualifiedTable;
  relColumns: Record<string, string>[];
  relDriver: string;
};

export const relSetName = (relName: string) => ({
  type: REL_SET_NAME as typeof REL_SET_NAME,
  relName,
});
export const relSetType = (relType: string) => ({
  type: REL_SET_TYPE as typeof REL_SET_TYPE,
  relType,
});
export const relSetSource = (relSource: string) => ({
  type: REL_SET_SOURCE as typeof REL_SET_SOURCE,
  relSource,
});
export const relSetTable = (relTable: QualifiedTable) => ({
  type: REL_SET_TABLE as typeof REL_SET_TABLE,
  relTable,
});
export const relSetColumns = (relColumns: Record<string, string>[]) => ({
  type: REL_SET_COLUMNS as typeof REL_SET_COLUMNS,
  relColumns,
});
export const relSetState = (relState: DbToDbRelState) => ({
  type: REL_SET_STATE as typeof REL_SET_STATE,
  relState,
});
export const relResetState = () => ({
  type: REL_RESET_STATE as typeof REL_RESET_STATE,
});
export const relSetDriver = (relDriver: string) => ({
  type: REL_SET_DRIVER as typeof REL_SET_DRIVER,
  relDriver,
});

type DbToDbRelEvents =
  | ReturnType<typeof relSetName>
  | ReturnType<typeof relSetType>
  | ReturnType<typeof relSetSource>
  | ReturnType<typeof relSetTable>
  | ReturnType<typeof relSetColumns>
  | ReturnType<typeof relSetState>
  | ReturnType<typeof relSetDriver>
  | ReturnType<typeof relResetState>;

export const dbToDbRelDefaultState: DbToDbRelState = {
  relName: '',
  relType: '',
  relSource: '',
  relTable: {
    name: '',
    schema: '',
  },
  relColumns: [{ column: '', refColumn: '' }],
  relDriver: '',
};

export const dbToDbRelReducer = (
  state = dbToDbRelDefaultState,
  action: DbToDbRelEvents
): DbToDbRelState => {
  switch (action.type) {
    case REL_SET_NAME:
      return {
        ...state,
        relName: action.relName,
      };
    case REL_SET_TYPE:
      return {
        ...state,
        relType: action.relType,
      };
    case REL_SET_SOURCE:
      return {
        ...state,
        relSource: action.relSource,
      };
    case REL_SET_TABLE:
      return {
        ...state,
        relTable: action.relTable,
      };
    case REL_SET_COLUMNS:
      return {
        ...state,
        relColumns: action.relColumns,
      };
    case REL_SET_STATE:
      return {
        ...action.relState,
      };
    case REL_RESET_STATE:
      return {
        ...dbToDbRelDefaultState,
      };
    case REL_SET_DRIVER:
      return {
        ...state,
        relDriver: action.relDriver,
      };
    default:
      return state;
  }
};
