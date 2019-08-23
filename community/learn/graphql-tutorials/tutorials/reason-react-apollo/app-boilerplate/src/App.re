[@react.component]
let make = () => {
  <div>
    <Navbar />
    <div className="container-fluid p-left-right-0">
      <div className="col-xs-12 col-md-9 p-left-right-0">
        <div className="col-xs-12 col-md-6 sliderMenu p-30">
          <TodoPrivateWrapper />
        </div>
        <div className="col-xs-12 col-md-6 sliderMenu p-30 bg-gray border-right">
          <TodoPublicWrapper />
        </div>
      </div>
      <div className="col-xs-12 col-md-3 p-left-right-0">
        <div className="col-xs-12 col-md-12 sliderMenu p-30 bg-gray">
          <OnlineUsersWrapper />
        </div>
      </div>
    </div>
  </div>
}
