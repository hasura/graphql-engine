import React from "react";
import Content from "@theme-original/DocSidebar/Desktop/Content";
import VersionedLink from "@site/src/components/VersionedLink";
import styles from "./customStyles.module.scss";
import useIsBrowser from "@docusaurus/useIsBrowser";

export default function ContentWrapper(props) {
	const isBrowser = useIsBrowser();
	const isCloudDocs =
		isBrowser && window.location.pathname.includes("/graphql/cloud/");
	const isCoreDocs =
		isBrowser && window.location.pathname.includes("/graphql/core/");
	return (
		<>
			<Content {...props} />
		</>
	);
}
