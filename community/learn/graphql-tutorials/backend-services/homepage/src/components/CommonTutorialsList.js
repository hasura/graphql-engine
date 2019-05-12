import React from 'react';
import PropTypes from 'prop-types';
import '../styles/styles.scss';
class CommonTutorialsList extends React.Component {
  render() {
    const path = require('../images/tutorial-path.svg');
    console.log(this.props)
    const listTutorial = this.props.tutorial.map((list, key) => {
      console.log(list);
      return (
        <a key={key} href={list.url} target={'_blank'}>
          <div className= {(list.comingSoon) ? list.disableBgClassName + ' disabledList' : list.bgClassName + ' listTutorial'}>
            <div className={'tutorialIconWrapper'}>
              {(list.comingSoon) ?
                <div className={'comingSoon'}>
                  Coming soon
                </div>
                : null
              }
            </div>
          </div>
        </a>
      )
    });
    return (
      <div className={'col-md-4 col-sm-12 col-xs-12'}>
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
  tutorial: PropTypes.array.isRequired,
  bgClassName: PropTypes.string.isRequired,
  subText: PropTypes.string.isRequired,
  disableBgClassName: PropTypes.string.isRequired,
}
export default CommonTutorialsList;
