open AddActions;
open CommonUtils;

[@react.component]
let make = (~alias: option(string), ~index: int, ~support: bool, ~dispatch) => {

  if (!support) {

    ReasonReact.null;

  } else {

    let styles = [%raw "require('../../../Common/TableCommon/Table.scss')"];

    let onAliasChange = (event) => {
      ReactEvent.Form.preventDefault(event);
      dispatch(
        setColumnAlias(
          ReactEvent.Form.target(event)##value,
          index
        )
      );
    };

    let aliasValue = switch (alias) {
      | Some(value) => value
      | None => ""
    };

    <span className={getClassName("inputDefault", styles) ++ " " ++ getClassName("defaultWidth", styles)}>
      <input
        type_="text"
        className={" form-control"}
        value=aliasValue
        onChange=onAliasChange
        placeholder="column_alias"
      />
    </span>
  }

};
