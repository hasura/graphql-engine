import React from 'react';
const githubIcon = require('./components/images/github.svg');
import './components/styles.css';

const GithubLink = ({link, text}) => {
	return (
		<a href={link} className="githubSection">
			<img className="githubIcon" src={githubIcon} alt="github"/>
			{text}
		</a>
	);
}

export default GithubLink;