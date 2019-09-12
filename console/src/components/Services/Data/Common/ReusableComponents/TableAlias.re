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
  ~deleteOnChange
) => {

  let (state, dispatch) = React.useReducer((curState, action) => switch(action) {
    | SetQueryExpanded(expanded) => {{ ...curState, queryExpanded: expanded }};
    | SetMutationExpanded(expanded) => {{ ...curState, mutationExpanded: expanded }}
  }, { queryExpanded: false, mutationExpanded: false });

  let getQuerySection = () => {
    let toggleQueryExpanded = (_) => dispatch(SetQueryExpanded(!state.queryExpanded));
    <div className=getStyle("add_mar_bottom_mid", None)>
      <div
        className={getStyle("add_mar_bottom_small", None) ++ " " ++ getStyle("display_flex", None) ++ " " ++ getStyle("cursorPointer", None)}
        onClick={toggleQueryExpanded}
      >
        <i
          className={"fa fa-chevron-" ++ (state.queryExpanded ? "down " : "right ") ++ getStyle("add_mar_right", None)}
        />
        <b> {ReasonReact.string("Query and Subscription")} </b>
      </div>
      {
        state.queryExpanded
        ?
        (
          <div className={getStyle("add_pad_left", None) ++ " " ++ getStyle("add_pad_right", None)}>
            <div className={getStyle("display_flex", None) ++ " row " ++ getStyle("add_mar_bottom_small", None)}>
              <div className={getStyle("add_mar_right", None) ++ " col-md-3"}>
                {ReasonReact.string("Select")}
              </div>
              <div className={"col-md-2"}>
                <input
                  type_="text"
                  value=select
                  className="form-control"
                  onChange={selectOnChange}
                />
              </div>
            </div>
            <div className={getStyle("display_flex", None) ++ " row " ++ getStyle("add_mar_bottom_small", None)}>
              <div className={getStyle("add_mar_right", None) ++ " col-md-3"}>
                {ReasonReact.string("Select by PK")}
              </div>
              <div className={"col-md-2"}>
                <input
                  type_="text"
                  value=selectByPk
                  className="form-control"
                  onChange={selectByPkOnChange}
                />
              </div>
            </div>
            <div className={getStyle("display_flex", None) ++ " row " ++ getStyle("add_mar_bottom_small", None)}>
              <div className={getStyle("add_mar_right", None) ++ " col-md-3"}>
                {ReasonReact.string("Select Aggregate")}
              </div>
              <div className={"col-md-2"}>
                <input
                  type_="text"
                  value=selectAgg
                  className="form-control"
                  onChange={selectAggOnChange}
                />
              </div>
            </div>
          </div>
        )
        :
        ReasonReact.null
      }
    </div>
  };

  let getMutationSection = () => {
    let toggleMutationExpanded = (_) => dispatch(SetMutationExpanded(!state.mutationExpanded));
    <div className={getStyle("add_mar_bottom", None)}>
      <div
        className={getStyle("add_mar_bottom_small", None) ++ " " ++ getStyle("display_flex", None) ++ " " ++ getStyle("cursorPointer", None)}
        onClick={toggleMutationExpanded}
      >
        <i
          className={"fa fa-chevron-" ++ (state.mutationExpanded ? "down " : "right ") ++ getStyle("add_mar_right", None)}
        />
        <b> {ReasonReact.string("Mutation")} </b>
      </div>
      {
        state.mutationExpanded
        ?
        (
          <div className={getStyle("add_pad_left", None) ++ " " ++ getStyle("add_pad_right", None)}>
            <div className={getStyle("display_flex", None) ++ " row " ++ getStyle("add_mar_bottom_small", None)}>
              <div className={getStyle("add_mar_right", None) ++ " col-md-3"}>
                {ReasonReact.string("Insert")}
              </div>
              <div className={"col-md-2"}>
                <input
                  type_="text"
                  value=insert
                  className="form-control"
                  onChange={insertOnChange}
                />
              </div>
            </div>
            <div className={getStyle("display_flex", None) ++ " row " ++ getStyle("add_mar_bottom_small", None)}>
              <div className={getStyle("add_mar_right", None) ++ " col-md-3"}>
                {ReasonReact.string("Update")}
              </div>
              <div className={"col-md-2"}>
                <input
                  type_="text"
                  value=update
                  className="form-control"
                  onChange={updateOnChange}
                />
              </div>
            </div>
            <div className={getStyle("display_flex", None) ++ " row " ++ getStyle("add_mar_bottom_small", None)}>
              <div className={getStyle("add_mar_right", None) ++ " col-md-3"}>
                {ReasonReact.string("Delete")}
              </div>
              <div className={"col-md-2"}>
                <input
                  type_="text"
                  value=delete
                  className="form-control"
                  onChange={deleteOnChange}
                />
              </div>
            </div>
          </div>
        )
        :
        ReasonReact.null
      }
    </div>
  };

  <div>
    <div className={getStyle("add_mar_bottom_mid", None)}>
      {getQuerySection()}
      {getMutationSection()}
    </div>
  </div>
};