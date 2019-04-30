import React from 'react';
import '../styles/styles.scss';

class Faq extends React.Component {
  constructor() {
    super();
    this.state = {
      topicCover: [
        {
          question: 'I don’t want to use DigitalOcean.',
          description: 'I want to deploy on <> Yes! You can deploy wherever you like as long as you can run a docker container! To deploy Hasura head to these docs.',
          num: '1',
        },
        {
          question: 'I don’t want to use Auth0.',
          description: 'My auth system is <> PasssportJS, Auth0, Firebase-auth, MyOwnAuth',
          num: '2',
        },
      ]
    };
  }
  render() {
    const conversation = require('../images/conversation.svg');
    const topic = this.state.topicCover.map((cover, index) => {
      return (
        <div key={index} className={'topicCoveredList wd100'}>
          <div className={'col-md-1 col-sm-1 col-xs-2 noPadd'}>
            <div className={'circle'}>
              {cover.num}
            </div>
          </div>
          <div className={'col-md-10 col-sm-10 col-xs-10 noPadd'}>
            <div className={'sectionDescription'}>
              { cover.question }
            </div>
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
          <div className={'faqWrapper wd100'}>
            <div className={'sectionHeader'}>
              Frequently Asked Questions
            </div>
            <div className={'topicCovered'}>
              <div className={'col-md-8 col-sm-8 col-xs-12 noPadd'}>
                { topic }
              </div>
              <div className={'col-md-4 col-sm-4 col-xs-12 noPadd'}>
                <div className={'topicCoveredImg'}>
                  <img className={'img-responsive'} src={conversation} alt={'conversation'}/>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    );
  }
}
export default Faq;
