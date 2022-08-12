import React from 'react'
import { withRouteData } from 'react-static'
import { Link } from '@reach/router'

const Authors = ({ author }) => {
  const authorList = author.map((item) => {
    return (
      <div key={item.id}>
        <Link to={"/blog/" + item.id}>{item.name}</Link>
      </div>
    );
  });
  return (
    <div>
      {/* STATIC DATA FROM THE BUILD */}
      <h1>Authors</h1>
      {authorList.length ? authorList : 'There are no authors available'}
    </div>
  )
}

export default withRouteData(Authors)

