[@react.component]

let make = (~isPublic: bool) => {
  <form className="formInput">
    <input
      className="input"
      placeholder="What needs to be done?"
    />
    <i className="inputMarker fa fa-angle-right" />
  </form>
}