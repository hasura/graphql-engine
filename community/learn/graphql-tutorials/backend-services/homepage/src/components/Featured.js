import React from 'react';
import '../styles/styles.scss';
const frontendWeekly = require('./images/frontend-weekly.png');
const jsk = require('./images/jsk.png');
const react = require('./images/react.png');
const vueNews = require('./images/vue-news.png');
class Featured extends React.Component {
  render() {
    return (
      <div className={'whiteBgColor commonSectionWrapper'}>
        <div className={'container noPadd'}>
          <div className={'featuredWrapper'}>
            <div className={'sectionHeader'}>
              We’ve been featured!
            </div>
            <div className={'purpleLineSeperator'}>
            </div>
            <div className={'featuredIconWrapper'}>
              <div className={'featuredIcon'}>
                <a href="https://frontendweekly.co/issues/152" target="_blank" rel="noopener noreferrer"><img src={frontendWeekly} alt={'Frontend Weekly'}/></a>
              </div>
              <div className={'featuredIcon'}>
                <a href="https://javascriptkicks.com/articles/202558/jsk-weekly-july-10-2019" target="_blank" rel="noopener noreferrer"><img src={jsk} alt={'JSK'}/></a>
              </div>
              <div className={'featuredIcon'}>
                <a href="https://react.statuscode.com/issues/138" target="_blank" rel="noopener noreferrer"><img src={react} alt={'React'}/></a>
              </div>
              <div className={'featuredIcon'}>
                <a href="https://news.vuejs.org/issues/142" target="_blank" rel="noopener noreferrer"><img src={vueNews} alt={'Vue News'}/></a>
              </div>
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default Featured;
