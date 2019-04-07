package metadata

type tableConfig struct {
	name string

	schemaName string

	isTracked bool

	relationShipSuggestions []relationshipSuggestion

	relationships []relationship

	columns []string

	uniqueRelNames []string
}

func newTable(name, schemaName string, columns []string) *tableConfig {
	tableInfo := &tableConfig{
		name:                    name,
		schemaName:              schemaName,
		relationShipSuggestions: make([]relationshipSuggestion, 0),
		relationships:           make([]relationship, 0),
		columns:                 columns,
		uniqueRelNames:          columns,
	}
	return tableInfo
}

func (t *tableConfig) SetIsTracked(isTracked bool) {
	t.isTracked = isTracked
}

func (t *tableConfig) GetIsTracked() bool {
	return t.isTracked
}

func (t *tableConfig) Track() hasuraDBQuery {
	return hasuraDBQuery{
		QueryType: trackTable,
		Args: tableArg{
			Name:   t.name,
			Schema: t.schemaName,
		},
	}
}

func (t *tableConfig) UnTrack() hasuraDBQuery {
	return hasuraDBQuery{
		QueryType: unTrackTable,
		Args: unTrackTableArg{
			Table: tableArg{
				Name:   t.name,
				Schema: t.schemaName,
			},
		},
	}
}

func (t *tableConfig) TrackRelationShipSuggestions() ([]hasuraDBQuery, []hasuraDBQuery) {
	up := make([]hasuraDBQuery, 0)
	down := make([]hasuraDBQuery, 0)
	for _, relationship := range t.relationShipSuggestions {
		relationship.RelName = relationship.generateName(t.columns)
		up = append(up, relationship.Track(t.name, t.schemaName))
		down = append(down, relationship.UnTrack(t.name, t.schemaName))
		t.columns = append(t.columns, relationship.RelName)
	}
	return up, down
}

func (t *tableConfig) UnTrackRelationShips() ([]hasuraDBQuery, []hasuraDBQuery) {
	up := make([]hasuraDBQuery, 0)
	down := make([]hasuraDBQuery, 0)
	for _, relationship := range t.relationships {
		up = append(up, relationship.UnTrack(t.name, t.schemaName))
		down = append(down, relationship.Track(t.name, t.schemaName))
	}
	return up, down
}
