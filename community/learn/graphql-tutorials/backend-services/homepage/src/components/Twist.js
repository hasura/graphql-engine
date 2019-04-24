import React from 'react';
import '../styles/styles.scss';
class Twist extends React.Component {
  render() {
    const laptop = require('../images/laptop.png');
    return (
      <div className={'commonSectionWrapper'}>
        <div className={'container noPadd'}>
          <div className={'twistWrapper'}>
            <div className={'sectionSubHeader'}>
              Here’s what you will be building
            </div>
            <div className={'sectionHeader'}>
              A twist to the good old to-do app
            </div>
            <div className={'gradientBtn'}>
              <a href="https://final-graphql-tutorial.netlify.com/">
                <button>TRY NOW</button>
              </a>
            </div>
            <div className={'twistImg'}>
              <img className={'img-responsive'} src={laptop} alt={'laptop'} />
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default Twist;
