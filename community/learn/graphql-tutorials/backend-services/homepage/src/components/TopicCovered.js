import React from 'react';
import '../styles/styles.scss';
const baselineDone = require('../images/baseline-done.svg');
class TopicCovered extends React.Component {
  constructor() {
    super();
    this.state = {
      topicCover: [
        {
          description: 'GraphQL basics',
          imgSrc: baselineDone,
        },
        {
          description: 'GraphQL queries, mutations and subscriptions',
          imgSrc: baselineDone,
        },
        {
          description: 'Using an existing auth system with your GraphQL APIs',
          imgSrc: baselineDone,
        },
        {
          description: 'GraphQL query variables and fragments',
          imgSrc: baselineDone,
        },
        {
          description: 'Building a realtime feed with your GraphQL client',
          imgSrc: baselineDone,
        },
        {
          description: 'GraphQL & UI modularisation techniques',
          imgSrc: baselineDone,
        },
      ]
    };
  }
  render() {
    const pencil = require('../images/pencil.svg');
    const topic = this.state.topicCover.map((cover, index) => {
      return (
        <div key={index} className={'topicCoveredList wd100'}>
          <div className={'col-md-1 col-sm-1 col-xs-2 noPadd'}>
            <div className={'circle'}>
              <img className={'img-responsive'} src={cover.imgSrc} alt={'Tick'}/>
            </div>
          </div>
          <div className={'col-md-10 col-sm-10 col-xs-10 noPadd'}>
            <div className={'sectionDescription'}>
              { cover.description }
            </div>
          </div>
        </div>
      );
    });
    return (
      <div className={'commonSectionWrapper gradientBgColor'}>
        <div className={'container noPadd'}>
          <div className={'topicCoveredWrapper wd100'}>
            <div className={'sectionHeader'}>
              Topics Covered
            </div>
            <div className={'topicCovered'}>
              <div className={'col-md-8 col-sm-8 col-xs-12 noPadd'}>
                { topic }
              </div>
              <div className={'col-md-4 col-sm-4 col-xs-12 noPadd'}>
                <div className={'topicCoveredImg'}>
                  <img className={'img-responsive'} src={pencil} alt={'pencil'}/>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    );
  }
}
export default TopicCovered;
