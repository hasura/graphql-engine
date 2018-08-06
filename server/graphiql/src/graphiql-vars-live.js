var query = `
subscription live_reaction {
  country {
    name
    votes {
      count: vote_count
    }
    comments {
      comment
    }
  }
}

mutation vote_for_france {
  insert_country_vote (objects: {country: "France"}) {
    affected_rows
  }
}

mutation vote_for_croatia {
  insert_country_vote (objects: {country: "Croatia"}) {
    affected_rows
  }
}

mutation comment_on_france {
  insert_comment (
    objects: {
      country: "France"
      comment: "rot in hell"
    }
  ) {
    affected_rows
  }
}

mutation comment_on_croatia {
  insert_comment (
    objects: {
      country: "Croatia"
      comment: "beautiful football"
    }
  ) {
    affected_rows
  }
}
`;
var variables=`
{
}
`;
exports.query = query;
exports.variables = variables;
