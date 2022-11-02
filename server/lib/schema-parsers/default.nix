{ mkDerivation, aeson, base, dependent-map, dependent-sum
, graphql-parser, hashable, hasura-error-message, hspec
, insert-ordered-containers, lens, lib, mtl, scientific, some
, template-haskell, text, th-lift, unordered-containers, uuid
, vector, witherable
}:
mkDerivation {
  pname = "hasura-schema-parsers";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base dependent-map dependent-sum graphql-parser hashable
    hasura-error-message insert-ordered-containers lens mtl scientific
    some template-haskell text th-lift unordered-containers uuid vector
    witherable
  ];
  testHaskellDepends = [
    aeson base dependent-map dependent-sum graphql-parser hashable
    hasura-error-message hspec insert-ordered-containers lens mtl
    scientific some template-haskell text th-lift unordered-containers
    uuid vector witherable
  ];
  homepage = "https://github.com/hasura/github-engine#readme";
  license = "unknown";
}
