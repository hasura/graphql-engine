import React from 'react'
import { withRouteData } from 'react-static'
import { Link } from '@reach/router'

export default withRouteData(({ articles }) => {
	const articlesList = articles.map((article, index) => {
		return (
			<div key={index}>
				<h3>{article.title}</h3>
				<p>{article.content}</p>
			</div>
		)}
	)
	return (
		<div>
			<Link to="/blog">{'<'} Back</Link>
			<br />
			{articlesList.length ? articlesList : 'There are no posts by this author'}
		</div>
	)}
)
