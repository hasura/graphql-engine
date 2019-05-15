import React from 'react';

const TwitterShare = () => {
    return(
        <React.Fragment>
            <a href="https://twitter.com/intent/tweet?&text=I just completed this Hasura backend course for frontend developers by @HasuraHQ. Check it out here - https://learn.hasura.io/graphql/hasura" target="_blank"><img className={'twitterIcon'} src={'https://img.icons8.com/color/48/000000/twitter.png'} alt={'Twitter'} /></a>
        </React.Fragment>
    )
};

export default TwitterShare;