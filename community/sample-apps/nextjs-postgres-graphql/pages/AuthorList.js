const AuthorList = ({ authors }) => (
  <div>
    {authors && authors.map((a, i) => (
      <div key={i}>
        <h2>{a.name}</h2>
      </div>
    ))}
  </div>
)

export default AuthorList;
