
open ModifyActions;
open CommonUtils;

[@react.component]
let make = (
  ~tableName,
  ~select,
  ~selectByPk,
  ~selectAgg,
  ~insert,
  ~update,
  ~delete,
  ~dispatch,
  ~existingAliases: Js.Dict.t(string)
) => {

  let onChange = (field, alias) => {
    dispatch(modifyRootFields(field, alias));
  };

  let collapsedContent = () => {

    let getRow = (label, property) => {

      let aliasValue = switch(Js.Dict.get(existingAliases, property)) {
        | Some(value) => value 
        | None => ""
      };

      <div className={"row " ++ getClassName("add_mar_bottom_mid", None)}>
        <div className="col-md-3">
          <b>{ReasonReact.string(label)}</b>
        </div>
        <div className="col-md-3">
          <input
            value={aliasValue}
            disabled=true
            placeholder={"unset"}
            readOnly=true
            className={"form-control"}
          />
        </div>
      </div>
    };

    <div>
      {getRow("Select", "select")}
      {getRow("Select by PK", "select_by_pk")}
      {getRow("Select Aggregate", "select_aggregate")}
      {getRow("Insert", "insert")}
      {getRow("Update", "update")}
      {getRow("Delete", "delete")}
    </div>
  };

  let expandedContent = () => {
    <TableAlias
      tableName={tableName}
      select={select}
      selectOnChange={
        (e) => {
          onChange("select", ReactEvent.Form.target(e)##value);
        }
      }
      selectByPk={selectByPk}
      selectByPkOnChange={
        (e) => {
          onChange("select_by_pk", ReactEvent.Form.target(e)##value);
        }
      }
      selectAgg={selectAgg}
      selectAggOnChange={
        (e) => {
          onChange("select_aggregate", ReactEvent.Form.target(e)##value);
        }
      }
      insert={insert}
      insertOnChange={
        (e) => {
          onChange("insert", ReactEvent.Form.target(e)##value);
        }
      }
      update={update}
      updateOnChange={
        (e) => {
          onChange("update", ReactEvent.Form.target(e)##value);
        }
      }
      delete={delete}
      deleteOnChange={
        (e) => {
          onChange("delete", ReactEvent.Form.target(e)##value);
        }
      }
      expanded={true}
    />
  };

  <div>
    <ExpandableEditor
      isCollapsable={true}
      property="root-field-alias"
      service="modify-table"
      editorExpanded=expandedContent
      editorCollapsed=collapsedContent
    />
  </div>

}