type TrackProperties = {
  label: string;
	pageUrl: string;
  response: 'YES' | 'NO';
};

export const saTrack = (eventName: string, properties: TrackProperties) => {
	// if window.analytics is not available - it's an issue with plugin `src/plugins/docusaurus-plugin-segment-analytics`
	window.analytics &&
		window.analytics.track<TrackProperties>(eventName, { ...properties, category: "docs" });
};
