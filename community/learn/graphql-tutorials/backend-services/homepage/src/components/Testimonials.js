import React from 'react';
import '../styles/styles.scss';
class Testimonials extends React.Component {
  constructor() {
    super();
    this.state = {
      testimonial: [
        {
          description: (<span>“This is one of the best tutorials I have seen for getting started with GraphQL and React. This is an incredible roadmap for learning these concepts in a linear and digestible way.</span>),
          img: 'https://graphql-engine-cdn.hasura.io/learn-hasura/assets/homepage/Eve-Porcello.png',
          name: 'Eve Porcello',
          twitterLink: 'https://twitter.com/eveporcello',
          designation: (<span>Instructor <b>@egghead.io</b></span>),
        },
        {
          description: (<span>“This is a really great tutorial for people keen to learn more about GraphQL <span role="img" aria-labelledby="emoji">🚀</span> I just went through the React one, but they have tutorials for Vue, iOS and RN too <span role="img" aria-labelledby="emoji">💙</span> <a href="https://twitter.com/hashtag/2Hours2GraphQL?src=hash" target="_blank" rel="noopener noreferrer">#2Hours2GraphQL</a>.</span>),
          img: 'https://graphql-engine-cdn.hasura.io/learn-hasura/assets/homepage/Sibylle.png',
          name: 'Sibylle',
          twitterLink: 'https://twitter.com/s_ibylle/status/1138143802831585280',
          designation: (<span>Typeface <b>@brandung</b></span>),
        },
        {
          description: (<span>Check out this GraphQL <a href="https://twitter.com/hashtag/ReasonML?src=hash" target="_blank" rel="noopener noreferrer">#ReasonML</a> course for Reason React developers by <a href="https://twitter.com/HasuraHQ" target="_blank" rel="noopener noreferrer">@HasuraHQ</a> <a href="https://learn.hasura.io/graphql/reason-react-apollo" target="_blank" rel="noopener noreferrer">https://learn.hasura.io/graphql/reason-react-apollo</a>... “Will this course teach ReasonReact concepts as well?” Hell yes. There are some programming patterns on display in this app that are different from what you see in generally.</span>),
          img: 'https://graphql-engine-cdn.hasura.io/learn-hasura/assets/homepage/Imani.png',
          name: 'Imani’s Father',
          twitterLink: 'https://twitter.com/_idkjs/status/1151765251991453696',
          designation: (<span>Freelance software developer</span>),
        },
      ]
    }
  }
  render() {
    const listWrapper = this.state.testimonial.map((list, index) => {
      return (
        <div key={index} className={'testimonialList'}>
          <div className={'quotes'}>
            <img src={'https://graphql-engine-cdn.hasura.io/learn-hasura/assets/homepage/quote.svg'} alt={'Quote'} />
          </div>
          <div className={'testimonialContent'}>
            {list.description}
          </div>
          <div className={'authorWrapper'}>
            <div className={'authorImg'}>
              <img src={list.img} alt={list.name} />
            </div>
            <div className={'author'}>
              <div className={'name'}>
                <a href={list.twitterLink} target={'_blank'} rel="noopener noreferrer">{list.name}</a>
              </div>
              <div className={'designation'}>
                {list.designation}
              </div>
            </div>
          </div>
        </div>
      );
    });
    return (
      /* Use global styles normally */
      <div className={'whiteBgColor commonSectionWrapper'}>
        <div className={'container noPadd'}>
          <div className={'testimoialsWrapper wd80'}>
            <div className={'sectionHeader'}>
              Testimonials
            </div>
            <div className={'purpleLineSeperator'}>
            </div>
            <div className={'testimonialListWrapper'}>
              {listWrapper}
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default Testimonials;
