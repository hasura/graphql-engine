import React from 'react';
import Link from "./link";
import './styles.css';
class NextPrevious extends React.Component {
  render() {
    const { mdx, nav } = this.props;
    let currentIndex;
    const currentPaginationInfo = nav.map((el, index) => {
      if (el.url === mdx.fields.slug) {
        currentIndex = index;
      }
    });
    const nextInfo = {};
    const previousInfo = {};
    if (currentIndex === undefined) { // index
      nextInfo.url = nav[0].url;
      nextInfo.title = nav[0].title;
      previousInfo.url = null;
      previousInfo.title = null;
      currentIndex = -1;
    } else if (currentIndex === 0) { // first page
      nextInfo.url = nav[currentIndex+1] ? nav[currentIndex+1].url : null;
      nextInfo.title = nav[currentIndex+1] ? nav[currentIndex+1].title : null;
      previousInfo.url = null;
      previousInfo.title = null;
    } else if (currentIndex === (nav.length-1)) { // last page
      nextInfo.url = null;
      nextInfo.title = null;
      previousInfo.url = nav[currentIndex-1] ? nav[currentIndex-1].url : null;
      previousInfo.title = nav[currentIndex-1] ? nav[currentIndex-1].title : null;
    } else if (currentIndex) { // any other page
      nextInfo.url = nav[currentIndex+1].url;
      nextInfo.title = nav[currentIndex+1].title;
      previousInfo.url = nav[currentIndex-1].url;
      previousInfo.title = nav[currentIndex-1].title;
    }
    return (
      <div className={'nextPreviousWrapper'}>
        {previousInfo.url && currentIndex >= 0 ? 
          (<Link to={nav[currentIndex-1].url} className={'previousBtn'}>
            <div className={'leftArrow'}>
              <svg preserveAspectRatio="xMidYMid meet" height="1em" width="1em" fill="none" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" stroke="currentColor" className="_13gjrqj"><g><line x1="19" y1="12" x2="5" y2="12"/><polyline points="12 19 5 12 12 5"/></g></svg>
            </div>
            <div className={'preRightWrapper'}>
              <div className={'smallContent'}>
                <span>Previous</span>
              </div>
              <div className={'nextPreviousTitle'}>
                <span>{nav[currentIndex-1].title}</span>
              </div>
            </div>
          </Link>) : null
        }
        {nextInfo.url && currentIndex >= 0 ?
          (<Link to={nav[currentIndex+1].url} className={'nextBtn'}>
            <div className={'nextRightWrapper'}>
              <div className={'smallContent'}>
                <span>Next</span>
              </div>
              <div className={'nextPreviousTitle'}>
                <span>{nav[currentIndex+1] && nav[currentIndex+1].title}</span>
              </div>
            </div>
            <div className={'rightArrow'}>
              <svg preserveAspectRatio="xMidYMid meet" height="1em" width="1em" fill="none" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" stroke="currentColor" className="_13gjrqj"><g><line x1="5" y1="12" x2="19" y2="12"/><polyline points="12 5 19 12 12 19"/></g></svg>
            </div>
          </Link>) : null
        }
      </div>
    );
  }
}

export default NextPrevious;
