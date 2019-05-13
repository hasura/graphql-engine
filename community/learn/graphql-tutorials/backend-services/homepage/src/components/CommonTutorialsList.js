import React from 'react';
import PropTypes from 'prop-types';
import '../styles/styles.scss';
class CommonTutorialsList extends React.Component {
  render() {
    const path = 'https://graphql-engine-cdn.hasura.io/learn-hasura/assets/homepage/tutorial-path.svg';
    let anchorClass;
    const listTutorial = this.props.tutorial.map((list, key) => {
      anchorClass = list.comingSoon ? 'noClick' : '';
      return (
        <a className={anchorClass} key={key} href={list.url} target={'_blank'} data-toggle="tooltip" title={list.name}>
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
  subText: PropTypes.string.isRequired,
}
export default CommonTutorialsList;
