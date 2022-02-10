import React, { useState, useEffect } from "react";

const LatestRelease = ({ prerelease }) => {
	const [releases, setReleases] = useState({
		latest: "v1.3.3",
		prerelease: "v2.1.0-beta.3",
	});

	useEffect(() => {
		(async () => {
			const response = await fetch(
				"https://releases.hasura.io/graphql-engine?agent=docs.hasura.io"
			);
			const responseJson = await response.json();
			console.log(response, responseJson);
			setReleases(responseJson);
		})();
	}, []);

	return (
		<span>
			{prerelease ? releases.prerelease : releases.latest}
		</span>
	);
};

export default LatestRelease;
