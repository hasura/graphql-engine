import React from 'react';
import PropTypes from 'prop-types';
import '../styles/styles.scss';
class CommonTutorialsList extends React.Component {
  render() {
    const path = require('../images/tutorial-path.svg');
    console.log(this.props.frontendTutorial);
    const listTutorial = this.props.frontendTutorial.map((list) => {
      return (
        <a href={list.url} target={'_blank'}>
          <div className={'listTutorial'}>
            <div className={'tutorialIconWrapper'}>
              <img className={'img-responsive'} src={list.imgSrc} alt={list.imgAlt} />
              {(list.comingSoon === '') ? null :
                <div className={'comingSoon'}>
                  {list.comingSoon}
                </div>
              }
            </div>
          </div>
        </a>
      )
    });
    return (
      <div className={'col-md-4 col-sm-4 col-xs-12'}>
        <div className={'tutorailsHeaderWrapper'}>
          <div className={'tutorialBgImg'}>
            <img className={'img-responsive'} src={path} alt={'path'} />
          </div>
          <div className={'tutorialHeader'}>
            {this.props.title} <span>{this.props.subText}</span>
          </div>
        </div>
        <div className={'tutorialListWrapper'}>
          {listTutorial}
        </div>
      </div>
    );
  }
}
CommonTutorialsList.propTypes = {
  title: PropTypes.string.isRequired,
  frontendTutorial: PropTypes.array.isRequired,
  subText: PropTypes.string.isRequired,
}
export default CommonTutorialsList;
