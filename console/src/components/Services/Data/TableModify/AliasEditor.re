
open ModifyActions;

// TODO typed dispatch

[@react.component]
let make = (
  ~tableName,
  ~schemaName,
  ~dispatch,
  ~existingAliases,
  ~existingColNames,
  ~newAliases,
  ~save
) => {

  let onChange = (field, alias) => {
    dispatch(modifyRootFields(field, alias));
  };

  let content = (cRootFieldAliases, disabled) => {
    <TableAlias
      select={cRootFieldAliases##select}
      selectOnChange={
        (e) => {
          onChange("select", ReactEvent.Form.target(e)##value);
        }
      }
      selectByPk={cRootFieldAliases##select_by_pk}
      selectByPkOnChange={
        (e) => {
          onChange("select_by_pk", ReactEvent.Form.target(e)##value);
        }
      }
      selectAgg={cRootFieldAliases##select_aggregate}
      selectAggOnChange={
        (e) => {
          onChange("select_aggregate", ReactEvent.Form.target(e)##value);
        }
      }
      insert={cRootFieldAliases##insert}
      insertOnChange={
        (e) => {
          onChange("insert", ReactEvent.Form.target(e)##value);
        }
      }
      update={cRootFieldAliases##update}
      updateOnChange={
        (e) => {
          onChange("update", ReactEvent.Form.target(e)##value);
        }
      }
      delete={cRootFieldAliases##delete}
      deleteOnChange={
        (e) => {
          onChange("delete", ReactEvent.Form.target(e)##value);
        }
      }
      expanded={true}
      disabled={disabled}
    />
  };

  let editorExpanded = () => content(newAliases, false);
  let editorCollapsed = () => content(existingAliases, true);

  let expandCallback = () => {
    onChange("select", existingAliases##select);
    onChange("select_by_pk", existingAliases##select_by_pk);
    onChange("select_aggregate", existingAliases##select_aggregate);
    onChange("insert", existingAliases##insert);
    onChange("update", existingAliases##update);
    onChange("delete", existingAliases##delete);
  };

  let saveFunc = (toggle) => {
    let (upQueries, downQueries) = generateAliasingQuery(
      newAliases,
      existingColNames,
      existingAliases,
      existingColNames,
      tableName,
      schemaName
    );
    save(upQueries, downQueries, toggle);
  };

  <div>
    <ExpandableEditor
      isCollapsable={true}
      property="root-field-alias"
      service="modify-table"
      editorExpanded
      editorCollapsed
      expandCallback
      saveFunc
    />
  </div>

}