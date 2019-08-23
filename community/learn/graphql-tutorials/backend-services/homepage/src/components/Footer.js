import React from 'react';
import '../styles/styles.scss';
import SubscribeFrom from 'react-mailchimp-subscribe';
const formProps = {
  action: '//hasura.us13.list-manage.com/subscribe/post?u=9b63e92a98ecdc99732456b0e&amp;id=f5c4f66bcf',
  messages: {
    inputPlaceholder: 'Your E-mail Address',
    btnLabel: 'SUBSCRIBE',
    sending: 'Subscribing...',
    success: 'Thank you for subscribing!',
    error: 'Invalid email or you have already subscribed for the newsletter'
  },
  styles: {
    sending: {
      fontSize: '15px',
      paddingTop: '10px',
      color: '#ccc'
    },
    success: {
      fontSize: '15px',
      paddingTop: '10px',
      color: '#FFCA27'
    },
    error: {
      fontSize: '15px',
      paddingTop: '10px',
      color: 'red'
    }
  }
};
class Footer extends React.Component {
  render() {
    const blueLamda = 'https://graphql-engine-cdn.hasura.io/learn-hasura/assets/homepage/Hasura.svg';
    return (
      <div id="footer" ref="footer" className={'darkGrayBgColor commonSectionWrapper wd100'}>
          <div className={'container noPadd'}>
            <div className={'footer_wrapper'}>
              <div className={'col-md-2  col-sm-12 col-xs-12'}>
                <div className={'logo_icon'}>
                  <img className={'img-responsive'} src={blueLamda} alt={'Lamda Icon'} />
                </div>
              </div>
              <div className={'col-md-2  col-sm-12 col-xs-12'}>
                <div className={'footer_section'}>
                  <div className={'footer_header'}>
                    RESOURCES
                  </div>
                  <div className={'footer_links'}>
                    <a href="https://blog.hasura.io/" target="_blank" rel="noopener noreferrer">Blog</a>
                  </div>
                  <div className={'footer_links'}>
                    <a href="https://learn.hasura.io/" target="_blank" rel="noopener noreferrer">GraphQL Tutorials</a>
                  </div>
                  <div className={'footer_links'}>
                    <a href="https://hasura.io/community" target="_blank" rel="noopener noreferrer">Community & Events</a>
                  </div>
                  <div className={'footer_links'}>
                    <a href="https://3factor.app/" target="_blank" rel="noopener noreferrer">3factor apps</a>
                  </div>
                  <div className={'footer_links'}>
                    <a href="https://hasura.io/diy-graphql-baas" target="_blank" rel="noopener noreferrer">DIY GraphQL BaaS</a>
                  </div>
                  <div className={'footer_links'}>
                    <a href="https://firebase2graphql.com/" target="_blank" rel="noopener noreferrer">Firebase to GraphQL</a>
                  </div>
                  <div className={'footer_links'}>
                    <a href="https://hasura.io/sample-apps" target="_blank" rel="noopener noreferrer">Sample Apps</a>
                  </div>
                  <div className={'footer_links'}>
                    <a href="https://hasura.io/3factor-radio" target="_blank" rel="noopener noreferrer">3factor Radio</a>
                  </div>
                  <div className={'footer_links'}>
                    <a href="https://hasura.io/vue-graphql" target="_blank" rel="noopener noreferrer">Vue GraphQL</a>
                  </div>
                  <div className={'footer_links'}>
                    <a href="https://hasura.io/react-graphql" target="_blank" rel="noopener noreferrer">React GraphQL</a>
                  </div>
                  <div className={'footer_links'}>
                    <a href="https://www.graphql-asia.org/" target="_blank" rel="noopener noreferrer">GraphQL Asia</a>
                  </div>
                </div>
              </div>
              <div className={'col-md-2  col-sm-12 col-xs-12'}>
                <div className={'footer_section'}>
                  <div className={'footer_header'}>
                    INFO
                  </div>
                  <div className={'footer_links'}>
                    <a href="https://hasura.io/about"target="_blank" rel="noopener noreferrer">About Us</a>
                  </div>
                  <div className={'footer_links'}>
                    <a href="https://hasura.io/careers"target="_blank" rel="noopener noreferrer">Careers</a>
                  </div>
                  <div className={'footer_links'}>
                    <a href="https://hasura.io/getintouch"target="_blank" rel="noopener noreferrer">Get In Touch</a>
                  </div>
                  <div className={'footer_links'}>
                    <a href="https://hasura.io/legal" target="_blank" rel="noopener noreferrer">Legal Stuff</a>
                  </div>
                  <div className={'footer_links'}>
                    <a href={'https://github.com/hasura/graphql-engine/blob/master/assets/brand'} target={'_blank'} rel="noopener noreferrer">Brand Assets</a>
                  </div>
                  <div className={'footer_links'}>
                    <a href="https://hasura.io/enterprise" target="_blank" rel="noopener noreferrer">Enterprise</a>
                  </div>
                </div>
              </div>
              <div className={'col-md-5  col-sm-12 col-xs-12 noPadd'}>
                <div className={'footer_section'}>
                  <div className={'footer_header'}>
                    FOLLOW US
                  </div>
                  <div className={'footer_social_wrapper'}>
                    <div className={'social_icons'}>
                      <a href={'https://www.facebook.com/HasuraHQ/'} target={'_blank'} >
                        <i className="fab fa-facebook" aria-hidden="true"/>
                      </a>
                    </div>
                    <div className={'social_icons'}>
                      <a href={'https://twitter.com/hasurahq/'} target={'_blank'}>
                        <i className="fab fa-twitter" aria-hidden="true"/>
                      </a>
                    </div>
                    <div className={'social_icons'}>
                      <a href={'https://www.instagram.com/hasurahq/'} target={'_blank'}>
                        <i className="fab fa-instagram" aria-hidden="true"/>
                      </a>
                    </div>
                    <div className={'social_icons'}>
                      <a href={'https://discord.gg/hasura'} target={'_blank'}>
                        <div className={'discordIcon'}></div>
                      </a>
                    </div>
                    <div className={'social_icons'}>
                      <a href={'https://github.com/hasura/graphql-engine'} target={'_blank'}>
                        <i className="fab fa-github" aria-hidden="true"></i>
                      </a>
                    </div>
                    <div className={'social_icons'}>
                      <a href={'https://www.twitch.tv/hasuraHQ'} target={'_blank'}>
                        <i className="fab fa-twitch" aria-hidden="true"></i>
                      </a>
                    </div>
                    <div className={'social_icons'}>
                      <a href={'https://www.youtube.com/channel/UCZo1ciR8pZvdD3Wxp9aSNhQ'} target={'_blank'}>
                        <i className="fab fa-youtube" aria-hidden="true"></i>
                      </a>
                    </div>
                  </div>
                  <div className={'footer_header'}>
                    Join Our Mailing List
                  </div>
                  <div className={'footer_email'}>
                    <SubscribeFrom className="subscribe_form subscribe_form3" {...formProps}/>
                  </div>
                </div>
              </div>
            </div>
            <div className={'copy_text'}>
              © 2019 Hasura Inc. All rights reserved
            </div>
          </div>
        </div>
    );
  }
}
export default Footer;
