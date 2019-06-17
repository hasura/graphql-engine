import React from 'react';

class Testimonials extends React.Component {
  render() {
    const styles = require('./Styles.module.scss');
    const twitter = require('./images/twitter.svg');
    const LeftArrow = require('./images/Left-Arrow.png');
    const RightArrow = require('./images/Right-Arrow.png');
    const quoteLeft = require('./images/quote-left.svg');
    const quoteRight = require('./images/quote-right.svg');
    return (
      /* Use global styles normally */
      <div className={styles.Testimonials}>
        <div className="container">
          <div className={styles.TestimonialsSection}>
            <div className={styles.TestimonialsWrapper}>
              <div className={styles.pageSubHeader}>
                Some testimonials from our users
              </div>
              <div className={styles.testimonialsCarouselWrapper}>
                <div id="myCarousel" className={'carousel slide ' + styles.testimonialsCarousel} data-ride="carousel" data-interval="false">
                  <div className={styles.quoteImg + ' ' + styles.quoteLeft}>
                    <img className={'img-responsive'} src={quoteLeft} alt={'Quote left'} />
                  </div>
                  <div className={styles.quoteImg + ' ' + styles.quoteright}>
                    <img className={'img-responsive'} src={quoteRight} alt={'Quote right'} />
                  </div>
                  <div className={'carousel-inner ' + styles.carouselInner}>
                    <div className={'item active ' + styles.carouselInnerItems}>
                      <div className={'col-md-6 col-sm-6 col-xs-12'}>
                          <div className={styles.indivRectBox}>
                            <div className={styles.quotes}>
                            </div>
                            <div className={styles.pageDescription}>
                              <a href="https://twitter.com/eveporcello" target="_blank" rel="noopener noreferrer">Eve Porcello, Instructor at Egghead.io</a>
                            </div>
                            <div className={styles.pageDescriptionSmall}>
                              This is one of the best tutorials I have seen for getting started with GraphQL and React. This is an incredible roadmap for learning these concepts in a linear and digestible way. If you are a React developer who is curious about Hasura and GraphQL, this is the place to start.
                            </div>
                          </div>
                      </div>
                      <div className={'col-md-6 col-sm-6 col-xs-12'}>
                          <div className={styles.indivRectBox}>
                            <div className={styles.quotes}>
                              <img className={'img-responsive'} src={twitter} alt={'twitter'} />
                            </div>
                            <div className={styles.pageDescription}>
                              <a href="https://twitter.com/raymondcamden/status/1131570271624802304" target="_blank" rel="noopener noreferrer">Raymond Camden, DevRel at American Express</a>
                            </div>
                            <div className={styles.pageDescriptionSmall}>
                              Damn, a free 2 hour course on GraphQL and <a href="https://twitter.com/vuejs" target="_blank" rel="noopener noreferrer">@vuejs</a> - <a href="https://learn.hasura.io/graphql/vue/introduction" target="_blank" rel="noopener noreferrer">https://learn.hasura.io/graphql/vue/introduction</a>. I can't wait to go through this! (When I can escape meeting hell.)
                            </div>
                          </div>
                      </div>
                      <div className={'col-md-6 col-sm-6 col-xs-12 visible-xs'}>
                        <div className={styles.indivRectBox}>
                          <div className={styles.quotes}>
                            <img className={'img-responsive'} src={twitter} alt={'twitter'} />
                          </div>
                          <div className={styles.pageDescription}>
                            <a href="https://twitter.com/themccallister/status/1127987752434376706" target="_blank" rel="noopener noreferrer">Jason M, Software Engineer</a>
                          </div>
                          <div className={styles.pageDescriptionSmall}>
                          Great introduction to <a href="https://twitter.com/hashtag/GraphQL?src=hash" target="_blank" rel="noopener noreferrer">#GraphQL</a> and covers <a href="https://twitter.com/HasuraHQ" target="_blank" rel="noopener noreferrer">@HasuraHQ</a>. If you’re new to either it’s a good read.
                          </div>
                        </div>
                      </div>
                      <div className={'col-md-6 col-sm-6 col-xs-12 visible-xs'}>
                          <div className={styles.indivRectBox}>
                            <div className={styles.quotes}>
                              <img className={'img-responsive'} src={twitter} alt={'twitter'} />
                            </div>
                            <div className={styles.pageDescription}>
                              <a href="https://twitter.com/kevinsimper/status/1129757964812869632" target="_blank" rel="noopener noreferrer">Kevin Simper, CopenhagenJS Organiser</a>
                            </div>
                            <div className={styles.pageDescriptionSmall}>
                              Use <a href="https://twitter.com/HasuraHQ" target="_blank" rel="noopener noreferrer">@hasurahq</a> to get a production ready self hosted graphql api for a postgres db, see their video course
                            </div>
                          </div>
                      </div>
                      <div className={'col-md-6 col-sm-6 col-xs-12 visible-xs'}>
                        <div className={styles.indivRectBox}>
                          <div className={styles.quotes}>
                            <img className={'img-responsive'} src={twitter} alt={'Twitter'} />
                          </div>
                          <div className={styles.pageDescription}>
                            <a href="https://twitter.com/GNUmanth/status/1129602954309193728" target="_blank" rel="noopener noreferrer">Hemanth, PayPal</a>
                          </div>
                          <div className={styles.pageDescriptionSmall}>
                           <a href="https://learn.hasura.io/graphql/react" target="_blank" rel="noopener noreferrer">learn.hasura.io/graphql/react</a> <a href="https://twitter.com/hashtag/GraphQL?src=hash" target="_blank" rel="noopener noreferrer">#GraphQL</a> course for <a href="https://twitter.com/hashtag/react?src=hash" target="_blank" rel="noopener noreferrer">#react</a> peeps is very well done by <a href="https://twitter.com/HasuraHQ" target="_blank" rel="noopener noreferrer">@HasuraHQ</a> 👌🤘
                          </div>
                        </div>
                      </div>
                      <div className={'col-md-6 col-sm-6 col-xs-12 visible-xs'}>
                          <div className={styles.indivRectBox}>
                            <div className={styles.quotes}>
                              <img className={'img-responsive'} src={twitter} alt={'twitter'} />
                            </div>
                            <div className={styles.pageDescription}>
                              <a href="https://twitter.com/cem2ran/status/1129782849958305795" target="_blank" rel="noopener noreferrer">Cem, React Native Engineer at Lenus.io</a>
                            </div>
                            <div className={styles.pageDescriptionSmall}>
                              I've heard great things about Hasura and also this training material
                            </div>
                          </div>
                      </div>
                      <div className={'col-md-6 col-sm-6 col-xs-12 visible-xs'}>
                          <div className={styles.indivRectBox}>
                            <div className={styles.quotes}>
                              <img className={'img-responsive'} src={twitter} alt={'twitter'} />
                            </div>
                            <div className={styles.pageDescription}>
                              <a href="https://twitter.com/MikhailSheen/status/1132524526909165570" target="_blank" rel="noopener noreferrer">Mikhail Sheen, Web Developer</a>
                            </div>
                            <div className={styles.pageDescriptionSmall}>
                              I just completed this GraphQL course for React developers by @HasuraHQ. Check it out here -
                              <a href="https://learn.hasura.io/graphql/react" target="_blank" rel="noopener noreferrer">https://learn.hasura.io/graphql/react</a> <p>That was awesome time spending.</p>
                                <a href="https://twitter.com/hashtag/GraphQL?src=hash" target="_blank" rel="noopener noreferrer">#GraphQL</a> <a href="https://twitter.com/hashtag/reactjs?src=hash" target="_blank" rel="noopener noreferrer">#reactjs</a> <a href="https://twitter.com/hashtag/react?src=hash" target="_blank" rel="noopener noreferrer">#react</a>
                            </div>
                          </div>
                      </div>
                      <div className={'col-md-6 col-sm-6 col-xs-12 visible-xs'}>
                          <div className={styles.indivRectBox}>
                            <div className={styles.quotes}>
                              <img className={'img-responsive'} src={twitter} alt={'twitter'} />
                            </div>
                            <div className={styles.pageDescription}>
                              <a href="https://twitter.com/s_ibylle/status/1138143802831585280" target="_blank" rel="noopener noreferrer">Sibylle, Front-End Dev @brandung</a>
                            </div>
                            <div className={styles.pageDescriptionSmall}>
                              This is a really great tutorial for people keen to learn more about GraphQL <span role="img" aria-labelledby="rocket">🚀</span> I just went through the React one, but they have tutorials for Vue, iOS and RN too <span role="img" aria-labelledby="heart">💙</span> Thanks for the folk at Hasura for putting that together <a href="https://twitter.com/hashtag/2Hours2GraphQL?src=hash" target="_blank" rel="noopener noreferrer">#2Hours2GraphQL</a>
                            </div>
                          </div>
                      </div>
                    </div>
                    <div className={'item hidden-xs ' + styles.carouselInnerItems}>
                      <div className={'col-md-6 col-sm-6 col-xs-12'}>
                        <div className={styles.indivRectBox}>
                          <div className={styles.quotes}>
                            <img className={'img-responsive'} src={twitter} alt={'twitter'} />
                          </div>
                          <div className={styles.pageDescription}>
                            <a href="https://twitter.com/themccallister/status/1127987752434376706" target="_blank" rel="noopener noreferrer">Jason M, Software Engineer</a>
                          </div>
                          <div className={styles.pageDescriptionSmall}>
                          Great introduction to <a href="https://twitter.com/hashtag/GraphQL?src=hash" target="_blank" rel="noopener noreferrer">#GraphQL</a> and covers <a href="https://twitter.com/HasuraHQ" target="_blank" rel="noopener noreferrer">@HasuraHQ</a>. If you’re new to either it’s a good read.
                          </div>
                        </div>
                      </div>
                      <div className={'col-md-6 col-sm-6 col-xs-12'}>
                          <div className={styles.indivRectBox}>
                            <div className={styles.quotes}>
                              <img className={'img-responsive'} src={twitter} alt={'twitter'} />
                            </div>
                            <div className={styles.pageDescription}>
                              <a href="https://twitter.com/kevinsimper/status/1129757964812869632" target="_blank" rel="noopener noreferrer">Kevin Simper, CopenhagenJS Organiser</a>
                            </div>
                            <div className={styles.pageDescriptionSmall}>
                              Use <a href="https://twitter.com/HasuraHQ" target="_blank" rel="noopener noreferrer">@hasurahq</a> to get a production ready self hosted graphql api for a postgres db, see their video course
                            </div>
                          </div>
                      </div>
                    </div>
                    <div className={'item hidden-xs ' + styles.carouselInnerItems}>
                      <div className={'col-md-6 col-sm-6 col-xs-12'}>
                        <div className={styles.indivRectBox}>
                          <div className={styles.quotes}>
                            <img className={'img-responsive'} src={twitter} alt={'Twitter'} />
                          </div>
                          <div className={styles.pageDescription}>
                            <a href="https://twitter.com/GNUmanth/status/1129602954309193728" target="_blank" rel="noopener noreferrer">Hemanth, PayPal</a>
                          </div>
                          <div className={styles.pageDescriptionSmall}>
                           <a href="https://learn.hasura.io/graphql/react" target="_blank" rel="noopener noreferrer">learn.hasura.io/graphql/react</a> <a href="https://twitter.com/hashtag/GraphQL?src=hash" target="_blank" rel="noopener noreferrer">#GraphQL</a> course for <a href="https://twitter.com/hashtag/react?src=hash" target="_blank" rel="noopener noreferrer">#react</a> peeps is very well done by <a href="https://twitter.com/HasuraHQ" target="_blank" rel="noopener noreferrer">@HasuraHQ</a> 👌🤘
                          </div>
                        </div>
                      </div>
                      <div className={'col-md-6 col-sm-6 col-xs-12'}>
                          <div className={styles.indivRectBox}>
                            <div className={styles.quotes}>
                              <img className={'img-responsive'} src={twitter} alt={'twitter'} />
                            </div>
                            <div className={styles.pageDescription}>
                              <a href="https://twitter.com/cem2ran/status/1129782849958305795" target="_blank" rel="noopener noreferrer">Cem, React Native Engineer at Lenus.io</a>
                            </div>
                            <div className={styles.pageDescriptionSmall}>
                              I've heard great things about Hasura and also this training material
                            </div>
                          </div>
                      </div>
                    </div>
                    <div className={'item hidden-xs ' + styles.carouselInnerItems}>
                      <div className={'col-md-6 col-sm-6 col-xs-12'}>
                          <div className={styles.indivRectBox}>
                            <div className={styles.quotes}>
                              <img className={'img-responsive'} src={twitter} alt={'twitter'} />
                            </div>
                            <div className={styles.pageDescription}>
                              <a href="https://twitter.com/MikhailSheen/status/1132524526909165570" target="_blank" rel="noopener noreferrer">Mikhail Sheen, Web Developer</a>
                            </div>
                            <div className={styles.pageDescriptionSmall}>
                              I just completed this GraphQL course for React developers by @HasuraHQ. Check it out here -
                              <a href="https://learn.hasura.io/graphql/react" target="_blank" rel="noopener noreferrer">https://learn.hasura.io/graphql/react</a> <p>That was awesome time spending.</p>
                                <a href="https://twitter.com/hashtag/GraphQL?src=hash" target="_blank" rel="noopener noreferrer">#GraphQL</a> <a href="https://twitter.com/hashtag/reactjs?src=hash" target="_blank" rel="noopener noreferrer">#reactjs</a> <a href="https://twitter.com/hashtag/react?src=hash" target="_blank" rel="noopener noreferrer">#react</a>
                            </div>
                          </div>
                      </div>
                      <div className={'col-md-6 col-sm-6 col-xs-12'}>
                          <div className={styles.indivRectBox}>
                            <div className={styles.quotes}>
                              <img className={'img-responsive'} src={twitter} alt={'twitter'} />
                            </div>
                            <div className={styles.pageDescription}>
                              <a href="https://twitter.com/s_ibylle/status/1138143802831585280" target="_blank" rel="noopener noreferrer">Sibylle, Front-End Dev @brandung</a>
                            </div>
                            <div className={styles.pageDescriptionSmall}>
                              This is a really great tutorial for people keen to learn more about GraphQL <span role="img" aria-labelledby="rocket">🚀</span> I just went through the React one, but they have tutorials for Vue, iOS and RN too <span role="img" aria-labelledby="heart">💙</span> Thanks for the folk at Hasura for putting that together <a href="https://twitter.com/hashtag/2Hours2GraphQL?src=hash" target="_blank" rel="noopener noreferrer">#2Hours2GraphQL</a>
                            </div>
                          </div>
                      </div>
                    </div>
                  </div>
                  <a className={'left carousel-control hidden-xs ' + styles.carouselControl} href="#myCarousel" data-slide="prev">
                    {/*
                    <span className="glyphicon glyphicon-chevron-left"></span>
                    */}
                    <span className="sr-only">Previous</span>
                    <img className={'img-responsive'} src={LeftArrow} alt={'Left arrow'} />
                  </a>
                  <a className={'right carousel-control hidden-xs ' + styles.carouselControl} href="#myCarousel" data-slide="next">
                    {/*
                    <span className="glyphicon glyphicon-chevron-right"></span>
                    <span className="sr-only">Next</span>
                    */}
                    <img className={'img-responsive'} src={RightArrow} alt={'Right arrow'} />
                  </a>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default Testimonials;
