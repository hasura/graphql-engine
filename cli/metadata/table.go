package metadata

type tableConfig struct {
	name string

	schemaName string

	isTracked bool

	relationShips []relationship

	columns []string

	uniqueRelNames []string
}

func newTable(name, schemaName string, columns []string) *tableConfig {
	tableInfo := &tableConfig{
		name:           name,
		schemaName:     schemaName,
		relationShips:  make([]relationship, 0),
		columns:        columns,
		uniqueRelNames: columns,
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

func (t *tableConfig) TrackRelationShips() ([]hasuraDBQuery, []hasuraDBQuery) {
	up := make([]hasuraDBQuery, 0)
	down := make([]hasuraDBQuery, 0)
	for _, relationship := range t.relationShips {
		if relationship.RelName != nil {
			continue
		}
		relName := relationship.generateName(t.columns)
		relationship.RelName = &relName

		up = append(up, relationship.Track(t.name, t.schemaName))
		down = append(down, relationship.UnTrack(t.name, t.schemaName))
		t.columns = append(t.columns, relName)
	}
	return up, down
}

func (t *tableConfig) UnTrackRelationShips() ([]hasuraDBQuery, []hasuraDBQuery) {
	up := make([]hasuraDBQuery, 0)
	down := make([]hasuraDBQuery, 0)
	for _, relationship := range t.relationShips {
		if relationship.RelName == nil {
			continue
		}
		up = append(up, relationship.UnTrack(t.name, t.schemaName))
		down = append(down, relationship.Track(t.name, t.schemaName))
	}
	return up, down
}
