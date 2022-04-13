import React, { useState, useEffect } from "react";

const LatestRelease = ({ prerelease }) => {
	const [releases, setReleases] = useState({
		latest: "v2.3.0",
		prerelease: "v2.4.0-beta.1",
	});

	useEffect(() => {
		(async () => {
			const response = await fetch(
				"https://releases.hasura.io/graphql-engine?agent=docs.hasura.io"
			);
			const responseJson = await response.json();
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
