import React from 'react';
import '../styles/styles.scss';
import Header from '../components/Header';
import TopBanner from '../components/TopBanner';
import Twist from '../components/Twist';
import TopicCovered from '../components/TopicCovered';
import ChooseTechnology from '../components/ChooseTechnology';
import Footer from '../components/Footer';
//  import Footer from '../components/Footer';
class Index extends React.Component {
  render() {
    return (
      <div className="wd100">
        <Header/>
        <TopBanner/>
        <Twist/>
        <TopicCovered/>
        <ChooseTechnology/>
        {/*<Faq/>*/}
         <Footer/>
      </div>
    );
  }
}


export default Index;
