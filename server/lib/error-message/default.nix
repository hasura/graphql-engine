{ mkDerivation, aeson, base, graphql-parser, hashable, hspec, lib
, text, unordered-containers, vector
}:
mkDerivation {
  pname = "hasura-error-message";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base graphql-parser text unordered-containers
  ];
  testHaskellDepends = [
    aeson base graphql-parser hashable hspec text unordered-containers
    vector
  ];
  homepage = "https://github.com/hasura/github-engine#readme";
  license = "unknown";
}
