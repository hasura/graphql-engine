open AddActions;

[@react.component]
let make = (
  ~tableName: string,
  ~select: string,
  ~selectByPk: string,
  ~selectAgg: string,
  ~insert: string,
  ~update: string,
  ~delete: string,
  ~dispatch
) => {

  let onChange = (field, value) => {
    dispatch(
      setRootFieldAlias(
        field,
        value
      )
    );
  };

  React.useEffect1(
    () => {
      if (tableName != "") {
        setDefaultAliases(tableName, dispatch);
      };
      None
    },
    [|tableName|]
  );

  <div>
    <TableAlias
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
      expanded=false
      disabled=false
    />
  </div>

};