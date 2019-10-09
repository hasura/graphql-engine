open Js.Dict;

let commonStyles: t(string) = [%raw "require('./Common.scss')" ];

let getClassName = (className, customStyles) => {
  switch (customStyles) {
    | Some(styles) => {
      switch(get(styles, className)) {
        | Some(style) => style;
        | None => "";
      }
    }
    | None => {
      switch(get(commonStyles, className)) {
        | Some (style) => style;
        | None => "";
      }
    }
  }
};

let fromOptionString = (optionVal) => switch(optionVal) {
  | Some(value) => value
  | None => ""
};

let stringToOption = (stringVal) => switch(stringVal) {
  | "" => None
  | _ => Some(stringVal)
};

let isUndefined : 'a => bool = (value) => {
  Js.Nullable.isNullable(value)
};