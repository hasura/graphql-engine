let commonStyles: Js.Dict.t(string) = [%raw "require('./Common.scss')" ];

let getStyle = (className, customStyles) => {
  switch (customStyles) {
    | Some(styles) => {
      switch(Js.Dict.get(styles, className)) {
        | Some(style) => style;
        | None => "";
      }
    }
    | None => {
      switch(Js.Dict.get(commonStyles, className)) {
        | Some (style) => style;
        | None => "";
      }
    }
  }
};
