import React from 'react';
import '../styles/styles.scss';
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
                <a href="https://frontendweekly.co/issues/152" target="_blank" rel="noopener noreferrer"><img src={'https://graphql-engine-cdn.hasura.io/learn-hasura/assets/homepage/frontend-weekly.png'} alt={'Frontend Weekly'}/></a>
              </div>
              <div className={'featuredIcon'}>
                <a href="https://javascriptkicks.com/articles/202558/jsk-weekly-july-10-2019" target="_blank" rel="noopener noreferrer"><img src={'https://graphql-engine-cdn.hasura.io/learn-hasura/assets/homepage/jsk.png'} alt={'JSK'}/></a>
              </div>
              <div className={'featuredIcon'}>
                <a href="https://react.statuscode.com/issues/138" target="_blank" rel="noopener noreferrer"><img src={'https://graphql-engine-cdn.hasura.io/learn-hasura/assets/homepage/react.png'} alt={'React Statuscode'}/></a>
              </div>
              <div className={'featuredIcon'}>
                <a href="https://news.vuejs.org/issues/142" target="_blank" rel="noopener noreferrer"><img src={'https://graphql-engine-cdn.hasura.io/learn-hasura/assets/homepage/vue-news.png'} alt={'Vue News'}/></a>
              </div>
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default Featured;
