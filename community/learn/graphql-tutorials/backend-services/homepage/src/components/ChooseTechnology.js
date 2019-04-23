import React from 'react';
import '../styles/styles.scss';
class ChooseTechnology extends React.Component {
  constructor() {
    super();
    this.state = {reactHover: false, reactNativeHover: false, vueHover: false, angularHover: false};
  }
  render() {
    // const containerDark = require('../images/container-dark.svg');
    const react = require('../images/react.svg');
    const vue = require('../images/vue.svg');
    const angular = require('../images/angular.svg');
    const android = require('../images/android.svg');
    const ios = require('../images/ios.png');
    const flutter = require('../images/flutter.png');
    const reasonml = require('../images/reasonml.jpg');
    // const circle = require('../images/circle.svg');
    const circleGray = require('../images/circle-gray.svg');

    const hoverHandler = (item, status) => {
      this.setState({ ...this.state, [item]:status})
    };

    return (
      <div className={'commonSectionWrapper'}>
        <div className={'container noPadd'}>
          <div className={'techologyWrapper'}>
            <div className={'sectionHeader'}>
              Choose your favourite technology
            </div>
            <div className={'chooseListWrapper'}>
              <div className={'col-md-3 col-sm-3 col-xs-12 mb-20'}>
                <a href="/graphql/react">
                  <div className={'chooseList'}>
                    <div className={'circle cirLeft'}>
                      <img className={'img-responsive'} src={circleGray} alt={'circle'}/>
                    </div>
                    <div className={'chooseListImg'}>
                      <img className={'img-responsive'} src={react} alt={'react logo'}/>
                    </div>
                  </div>
                  <div className={'techNameGray'}>
                    REACT JS + GRAPHQL <br/>
                  </div>
                </a>
              </div>
              <div className={'col-md-3 col-sm-3 col-xs-12 mb-20'}>
                <div className={'chooseListDisable'}>
                  <div className={'circle cirBottom'}>
                    <img className={'img-responsive'} src={circleGray} alt={'circle'}/>
                  </div>
                  <div className={'chooseListImg'}>
                    <img className={'img-responsive'} src={react} alt={'react logo'}/>
                  </div>
                </div>
                <div className={'techNameGray'}>
                  REACT NATIVE + GRAPHQL <br/>COMING SOON!
                </div>
              </div>
              <div className={'col-md-3 col-sm-3 col-xs-12 mb-20'}>
                <div className={'chooseListDisable'}>
                  <div className={'circle cirRight'}>
                    <img className={'img-responsive'} src={circleGray} alt={'circle'}/>
                  </div>
                  <div className={'chooseListImg'}>
                    <img className={'img-responsive'} src={vue} alt={'vue logo'}/>
                  </div>
                </div>
                <div className={'techNameGray'}>
                  VUE JS + GRAPHQL <br/>COMING SOON!
                </div>
              </div>
              <div className={'col-md-3 col-sm-3 col-xs-12 mb-20'}>
                <div className={'chooseListDisable'}>
                  <div className={'circle cirBottom1'}>
                    <img className={'img-responsive'} src={circleGray} alt={'circle'}/>
                  </div>
                  <div className={'chooseListImg'}>
                    <img className={'img-responsive'} src={angular} alt={'angular logo'}/>
                  </div>
                </div>
                <div className={'techNameGray'}>
                  ANGULAR + GRAPHQL <br/>COMING SOON!
                </div>
              </div>
            </div>
            <div className={'chooseListWrapper'}>
              <div className={'col-md-3 col-sm-3 col-xs-12 mb-20'}>
                <div className={'chooseListDisable'}>
                  <div className={'circle cirLeft'}>
                    <img className={'img-responsive'} src={circleGray} alt={'circle'}/>
                  </div>
                  <div className={'chooseListImg'}>
                    <img className={'img-responsive'} src={android} alt={'android logo'}/>
                  </div>
                </div>
                <div className={'techNameGray'}>
                  ANDROID + GRAPHQL <br/>COMING SOON!
                </div>
              </div>
              <div className={'col-md-3 col-sm-3 col-xs-12 mb-20'}>
                <div className={'chooseListDisable'}>
                  <div className={'circle cirBottom'}>
                    <img className={'img-responsive'} src={circleGray} alt={'circle'}/>
                  </div>
                  <div className={'chooseListImg'}>
                    <img className={'img-responsive'} src={ios} alt={'ios logo'}/>
                  </div>
                </div>
                <div className={'techNameGray'}>
                  iOS + GRAPHQL <br/>COMING SOON!
                </div>
              </div>
              <div className={'col-md-3 col-sm-3 col-xs-12 mb-20'}>
                <div className={'chooseListDisable'}>
                  <div className={'circle cirRight'}>
                    <img className={'img-responsive'} src={circleGray} alt={'circle'}/>
                  </div>
                  <div className={'chooseListImg'}>
                    <img className={'img-responsive'} src={flutter} alt={'flutter logo'}/>
                  </div>
                </div>
                <div className={'techNameGray'}>
                  FLUTTER + GRAPHQL <br/>COMING SOON!
                </div>
              </div>
              <div className={'col-md-3 col-sm-3 col-xs-12 mb-20'}>
                <div className={'chooseListDisable'}>
                  <div className={'circle cirBottom1'}>
                    <img className={'img-responsive'} src={circleGray} alt={'circle'}/>
                  </div>
                  <div className={'chooseListImg'}>
                    <img className={'img-responsive'} src={reasonml} alt={'reason ml logo'}/>
                  </div>
                </div>
                <div className={'techNameGray'}>
                  REASON ML + GRAPHQL <br/>COMING SOON!
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default ChooseTechnology;
