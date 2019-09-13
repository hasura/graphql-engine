open CommonUtils;

type state = {
  queryExpanded: bool,
  mutationExpanded: bool
};

type action =
  | SetQueryExpanded(bool)
  | SetMutationExpanded(bool);

[@react.component]
let make = (
  ~tableName: string,
  ~select: string,
  ~selectOnChange,
  ~selectByPk: string,
  ~selectByPkOnChange,
  ~selectAgg: string,
  ~selectAggOnChange,
  ~insert: string,
  ~insertOnChange,
  ~update: string,
  ~updateOnChange,
  ~delete: string,
  ~deleteOnChange,
  ~expanded: bool
) => {

  let (state, dispatch) = React.useReducer((curState, action) => switch(action) {
    | SetQueryExpanded(expanded) => {{ ...curState, queryExpanded: expanded }};
    | SetMutationExpanded(expanded) => {{ ...curState, mutationExpanded: expanded }}
  }, { queryExpanded: expanded, mutationExpanded: expanded });

  let getRow = (label, value, onChange) => {
    <div className={getClassName("display_flex", None) ++ " row " ++ getClassName("add_mar_bottom_small", None)}>
      <div className={getClassName("add_mar_right", None) ++ " col-md-3"}>
        {ReasonReact.string(label)}
      </div>
      <div className={"col-md-3"}>
        <input
          type_="text"
          value=value
          className="form-control"
          onChange={onChange}
        />
      </div>
    </div>
  };

  let getQuerySection = () => {
    let toggleQueryExpanded = (_) => dispatch(SetQueryExpanded(!state.queryExpanded));
    <div className=getClassName("add_mar_bottom_mid", None)>
      <div
        className={getClassName("add_mar_bottom_small", None) ++ " " ++ getClassName("display_flex", None) ++ " " ++ getClassName("cursorPointer", None)}
        onClick={toggleQueryExpanded}
      >
        <i
          className={"fa fa-chevron-" ++ (state.queryExpanded ? "down " : "right ") ++ getClassName("add_mar_right", None)}
        />
        <b> {ReasonReact.string("Query and Subscription")} </b>
      </div>
      {
        state.queryExpanded
        ?
        (
          <div className={getClassName("add_pad_left", None) ++ " " ++ getClassName("add_pad_right", None)}>
            {getRow("Select", select, selectOnChange)}
            {getRow("Select by PK", selectByPk, selectByPkOnChange)}
            {getRow("Select Aggregate", selectAgg, selectAggOnChange)}
          </div>
        )
        :
        ReasonReact.null
      }
    </div>
  };

  let getMutationSection = () => {
    let toggleMutationExpanded = (_) => dispatch(SetMutationExpanded(!state.mutationExpanded));
    <div className={getClassName("add_mar_bottom", None)}>
      <div
        className={getClassName("add_mar_bottom_small", None) ++ " " ++ getClassName("display_flex", None) ++ " " ++ getClassName("cursorPointer", None)}
        onClick={toggleMutationExpanded}
      >
        <i
          className={"fa fa-chevron-" ++ (state.mutationExpanded ? "down " : "right ") ++ getClassName("add_mar_right", None)}
        />
        <b> {ReasonReact.string("Mutation")} </b>
      </div>
      {
        state.mutationExpanded
        ?
        (
          <div className={getClassName("add_pad_left", None) ++ " " ++ getClassName("add_pad_right", None)}>
            {getRow("Insert", insert, insertOnChange)}
            {getRow("Update", update, updateOnChange)}
            {getRow("Delete", delete, deleteOnChange)}
          </div>
        )
        :
        ReasonReact.null
      }
    </div>
  };

  <div>
    <div className={getClassName("add_mar_bottom_mid", None)}>
      {getQuerySection()}
      {getMutationSection()}
    </div>
  </div>
};