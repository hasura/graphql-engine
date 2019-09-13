[@bs.module "./Editor.js"][@react.component]
external make: (
  ~toggled: bool=?,
  ~isCollapsable: bool=?,
  ~property: string,
  ~service: string,
  ~ongoingRequest: string=?,
  ~saveButtonColor: string=?,
  ~saveButtonText: string=?,
  ~saveFunc: (unit => unit) => unit = ?,
  ~removeButtonColor: string = ?,
  ~removeButtonText: string=?,
  ~removeFunc: (unit => unit) => unit = ?,
  ~collapseButtonText:string=?,
  ~collapseCallback: unit => unit = ?,
  ~expandCallback: unit => unit = ?,
  ~editorCollapsed: unit => React.element = ?,
  ~editorExpanded: unit => React.element = ?,
  ~collapsedLabel: unit => React.element = ?,
  ~expandedLabel: unit => React.element = ?
) => React.element = "default";