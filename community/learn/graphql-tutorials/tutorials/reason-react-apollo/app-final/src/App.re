[@react.component]
let make = () => {
  <ReasonApollo.Provider client=ApolloClient.instance>
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
        <ReasonApollo.Consumer>
          {
            client => {
              <OnlineUsersWrapper client={client}/>
            }
          }
        </ReasonApollo.Consumer>
        </div>
      </div>
    </div>
  </ReasonApollo.Provider>
}
