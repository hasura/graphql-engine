package hasuradb

import (
	"container/list"
	"fmt"
	"io"
	"io/ioutil"

	"github.com/hasura/graphql-engine/cli/migrate/database"
	"github.com/pkg/errors"

	"github.com/ahmetb/go-linq"

	yaml "github.com/ghodss/yaml"
	"github.com/qor/transition"
)

type CustomQuery linq.Query

func (q CustomQuery) MergeEventTriggers(squashList *database.CustomList) error {
	eventTriggerTransition := transition.New(&eventTriggerConfig{})
	eventTriggerTransition.Initial("new")
	eventTriggerTransition.State("created")
	eventTriggerTransition.State("deleted")

	eventTriggerTransition.Event("create_event_trigger").To("created").From("new", "deleted")
	eventTriggerTransition.Event("delete_event_trigger").To("deleted").From("new", "created")

	next := q.Iterate()

	for item, ok := next(); ok; item, ok = next() {
		var wasCreated bool
		g := item.(linq.Group)
		if g.Key == "" {
			continue
		}
		evKey := g.Key.(string)
		evCfg := eventTriggerConfig{
			name: evKey,
		}
		prevElems := make([]*list.Element, 0)
		for _, val := range g.Group {
			element := val.(*list.Element)
			switch obj := element.Value.(type) {
			case *createEventTriggerInput:
				if obj.Replace != nil {
					if *obj.Replace {
						for _, e := range prevElems {
							squashList.Remove(e)
						}
						err := eventTriggerTransition.Trigger("delete_event_trigger", &evCfg, nil)
						if err != nil {
							return err
						}
						obj.Replace = nil
					}
				} else {
					wasCreated = true
				}
				err := eventTriggerTransition.Trigger("create_event_trigger", &evCfg, nil)
				if err != nil {
					return err
				}
				prevElems = append(prevElems, element)
			case *deleteEventTriggerInput:
				if wasCreated {
					// if this is true it means that a trigger was created
					// which means their is no point in keeping the delete event trigger around
					//
					// otherwise it means that it was only updated so we have to keep
					// the delete trigger migration
					prevElems = append(prevElems, element)
				}
				err := eventTriggerTransition.Trigger("delete_event_trigger", &evCfg, nil)
				if err != nil {
					return err
				}
				// drop previous elements
				for _, e := range prevElems {
					squashList.Remove(e)
				}
			}
		}
	}
	return nil
}

func (q CustomQuery) MergeRelationships(squashList *database.CustomList) error {
	relationshipTransition := transition.New(&relationshipConfig{})
	relationshipTransition.Initial("new")
	relationshipTransition.State("created")
	relationshipTransition.State("dropped")

	relationshipTransition.Event("create_relationship").To("created").From("new", "dropped")
	relationshipTransition.Event("drop_relationship").To("dropped").From("new", "created")

	next := q.Iterate()

	for item, ok := next(); ok; item, ok = next() {
		g := item.(linq.Group)
		if g.Key == nil {
			continue
		}
		relKey := g.Key.(relationshipMap)
		relCfg := relationshipConfig{
			tableName:  relKey.tableName,
			schemaName: relKey.schemaName,
			name:       relKey.name,
		}
		prevElems := make([]*list.Element, 0)
		for _, val := range g.Group {
			element := val.(*list.Element)
			switch obj := element.Value.(type) {
			case *createObjectRelationshipInput:
				err := relationshipTransition.Trigger("create_relationship", &relCfg, nil)
				if err != nil {
					return errors.Wrap(err, fmt.Sprintf("relationship %s %s", obj.Name, relKey.name))
				}
				prevElems = append(prevElems, element)
			case *createArrayRelationshipInput:
				err := relationshipTransition.Trigger("create_relationship", &relCfg, nil)
				if err != nil {
					return errors.Wrap(err, fmt.Sprintf("relationship %s %s", obj.Name, relKey.name))
				}
				prevElems = append(prevElems, element)
			case *setRelationshipCommentInput:
				if len(prevElems) != 0 {
					if rel, ok := prevElems[0].Value.(*createObjectRelationshipInput); ok {
						rel.Comment = obj.Comment
						continue
					}

					if rel, ok := prevElems[0].Value.(*createArrayRelationshipInput); ok {
						rel.Comment = obj.Comment
						continue
					}
				}
				prevElems = append(prevElems, element)
			case *dropRelationshipInput:
				if relCfg.GetState() == "created" {
					prevElems = append(prevElems, element)
				}
				err := relationshipTransition.Trigger("drop_relationship", &relCfg, nil)
				if err != nil {
					return err
				}
				// drop previous elements
				for _, e := range prevElems {
					squashList.Remove(e)
				}
			}
		}
	}
	return nil
}

func (q CustomQuery) MergePermissions(squashList *database.CustomList) error {
	permissionTransition := transition.New(&permissionConfig{})
	permissionTransition.Initial("new")
	permissionTransition.State("created")
	permissionTransition.State("dropped")

	permissionTransition.Event("create_permission").To("created").From("new", "dropped")
	permissionTransition.Event("drop_permission").To("dropped").From("new", "created")

	next := q.Iterate()

	for item, ok := next(); ok; item, ok = next() {
		g := item.(linq.Group)
		if g.Key == nil {
			continue
		}
		permKey := g.Key.(permissionMap)
		permCfg := permissionConfig{
			tableName:  permKey.tableName,
			schemaName: permKey.schemaName,
			permType:   permKey.permType,
			role:       permKey.Role,
		}
		prevElems := make([]*list.Element, 0)
		for _, val := range g.Group {
			element := val.(*list.Element)
			switch obj := element.Value.(type) {
			case *createInsertPermissionInput, *createSelectPermissionInput, *createUpdatePermissionInput, *createDeletePermissionInput:
				err := permissionTransition.Trigger("create_permission", &permCfg, nil)
				if err != nil {
					return err
				}
				prevElems = append(prevElems, element)
			case *setPermissionCommentInput:
				if len(prevElems) != 0 {
					if perm, ok := prevElems[0].Value.(*createInsertPermissionInput); ok {
						perm.Comment = obj.Comment
						continue
					}

					if perm, ok := prevElems[0].Value.(*createSelectPermissionInput); ok {
						perm.Comment = obj.Comment
						continue
					}

					if perm, ok := prevElems[0].Value.(*createUpdatePermissionInput); ok {
						perm.Comment = obj.Comment
						continue
					}

					if perm, ok := prevElems[0].Value.(*createDeletePermissionInput); ok {
						perm.Comment = obj.Comment
						continue
					}
				}
				prevElems = append(prevElems, element)
			case *dropInsertPermissionInput, *dropSelectPermissionInput, *dropUpdatePermissionInput, *dropDeletePermissionInput:
				if permCfg.GetState() == "created" {
					prevElems = append(prevElems, element)
				}
				err := permissionTransition.Trigger("drop_permission", &permCfg, nil)
				if err != nil {
					return err
				}
				// drop previous elements
				for _, e := range prevElems {
					squashList.Remove(e)
				}
			}
		}
	}
	return nil
}

func (q CustomQuery) MergeComputedFields(squashList *database.CustomList) error {
	computedFieldTransition := transition.New(&computedFieldConfig{})
	computedFieldTransition.Initial("new")
	computedFieldTransition.State("added")
	computedFieldTransition.State("dropped")

	computedFieldTransition.Event("add_computed_field").To("added").From("new", "dropped")
	computedFieldTransition.Event("drop_computed_field").To("dropped").From("new", "added")

	next := q.Iterate()

	for item, ok := next(); ok; item, ok = next() {
		g := item.(linq.Group)
		if g.Key == nil {
			continue
		}
		cfKey := g.Key.(computedFieldMap)
		cfCfg := computedFieldConfig{
			tableName:  cfKey.tableName,
			schemaName: cfKey.schemaName,
			name:       cfKey.name,
		}
		prevElems := make([]*list.Element, 0)
		for _, val := range g.Group {
			element := val.(*list.Element)
			switch obj := element.Value.(type) {
			case *addComputedFieldInput:
				err := computedFieldTransition.Trigger("add_computed_field", &cfCfg, nil)
				if err != nil {
					return errors.Wrap(err, fmt.Sprintf("computed field %s", obj.Name))
				}
				prevElems = append(prevElems, element)
			case *dropComputedFieldInput:
				if cfCfg.GetState() == "added" {
					prevElems = append(prevElems, element)
				}
				err := computedFieldTransition.Trigger("drop_computed_field", &cfCfg, nil)
				if err != nil {
					return err
				}
				// drop previous elements
				for _, e := range prevElems {
					squashList.Remove(e)
				}
			}
		}
	}
	return nil
}

func (q CustomQuery) MergeTableCustomFields(squashList *database.CustomList) error {
	next := q.Iterate()

	for item, ok := next(); ok; item, ok = next() {
		g := item.(linq.Group)
		if g.Key == nil {
			continue
		}
		var prevElem *list.Element
		for _, val := range g.Group {
			element := val.(*list.Element)
			switch element.Value.(type) {
			case *setTableCustomFieldsV2Input:
				if prevElem != nil {
					squashList.Remove(prevElem)
				}
				prevElem = element
			}
		}
	}
	return nil
}

func (q CustomQuery) MergeTables(squashList *database.CustomList) error {
	tableTransition := transition.New(&tableConfig{})
	tableTransition.Initial("new")
	tableTransition.State("tracked")
	tableTransition.State("untracked")

	tableTransition.Event("track_table").To("tracked").From("new", "untracked")
	tableTransition.Event("untrack_table").To("untracked").From("new", "tracked")

	next := q.Iterate()

	for item, ok := next(); ok; item, ok = next() {
		g := item.(linq.Group)
		if g.Key == nil {
			continue
		}
		tblKey := g.Key.(tableMap)
		tblCfg := tableConfig{
			name:   tblKey.name,
			schema: tblKey.schema,
		}
		prevElems := make([]*list.Element, 0)
		for _, val := range g.Group {
			element := val.(*list.Element)
			switch args := element.Value.(type) {
			case *trackTableInput, *trackTableV2Input:
				err := tableTransition.Trigger("track_table", &tblCfg, nil)
				if err != nil {
					return err
				}
				prevElems = append(prevElems, element)
			case *unTrackTableInput:
				if tblCfg.GetState() == "tracked" {
					prevElems = append(prevElems, element)
				}
				err := tableTransition.Trigger("untrack_table", &tblCfg, nil)
				if err != nil {
					return err
				}
				// drop previous elements
				for _, e := range prevElems {
					squashList.Remove(e)
				}
			case *createEventTriggerInput:
				if tblCfg.GetState() == "untracked" {
					return fmt.Errorf("cannot create event trigger %s when table %s on schema %s is untracked", args.Name, tblCfg.name, tblCfg.schema)
				}
				prevElems = append(prevElems, element)
			case *deleteEventTriggerInput:
				if tblCfg.GetState() == "untracked" {
					return fmt.Errorf("cannot delete event trigger %s when table %s on schema %s is untracked", args.Name, tblCfg.name, tblCfg.schema)
				}
				prevElems = append(prevElems, element)
			case *createObjectRelationshipInput:
				if tblCfg.GetState() == "untracked" {
					return fmt.Errorf("cannot create object relationship %s when table %s on schema %s is untracked", args.Name, tblCfg.name, tblCfg.schema)
				}
				prevElems = append(prevElems, element)
			case *createArrayRelationshipInput:
				if tblCfg.GetState() == "untracked" {
					return fmt.Errorf("cannot create array relationship %s when table %s on schema %s is untracked", args.Name, tblCfg.name, tblCfg.schema)
				}
				prevElems = append(prevElems, element)
			case *setRelationshipCommentInput:
				prevElems = append(prevElems, element)
			case *dropRelationshipInput:
				if tblCfg.GetState() == "untracked" {
					return fmt.Errorf("cannot drop relationship %s when table %s on schema %s is untracked", args.RelationShip, tblCfg.name, tblCfg.schema)
				}
				prevElems = append(prevElems, element)
			case *createInsertPermissionInput:
				if tblCfg.GetState() == "untracked" {
					return fmt.Errorf("cannot create insert permission for %s role when table %s on schema %s is untracked", args.Role, tblCfg.name, tblCfg.schema)
				}
				prevElems = append(prevElems, element)
			case *createSelectPermissionInput:
				if tblCfg.GetState() == "untracked" {
					return fmt.Errorf("cannot create select permission for %s role when table %s on schema %s is untracked", args.Role, tblCfg.name, tblCfg.schema)
				}
				prevElems = append(prevElems, element)
			case *createUpdatePermissionInput:
				if tblCfg.GetState() == "untracked" {
					return fmt.Errorf("cannot create update permission for %s role when table %s on schema %s is untracked", args.Role, tblCfg.name, tblCfg.schema)
				}
				prevElems = append(prevElems, element)
			case *createDeletePermissionInput:
				if tblCfg.GetState() == "untracked" {
					return fmt.Errorf("cannot create delete permission for %s role when table %s on schema %s is untracked", args.Role, tblCfg.name, tblCfg.schema)
				}
				prevElems = append(prevElems, element)
			case *setPermissionCommentInput:
				prevElems = append(prevElems, element)
			case *dropInsertPermissionInput:
				if tblCfg.GetState() == "untracked" {
					return fmt.Errorf("cannot drop insert permission for %s role when table %s on schema %s is untracked", args.Role, tblCfg.name, tblCfg.schema)
				}
				prevElems = append(prevElems, element)
			case *dropSelectPermissionInput:
				if tblCfg.GetState() == "untracked" {
					return fmt.Errorf("cannot drop select permission for %s role when table %s on schema %s is untracked", args.Role, tblCfg.name, tblCfg.schema)
				}
				prevElems = append(prevElems, element)
			case *dropUpdatePermissionInput:
				if tblCfg.GetState() == "untracked" {
					return fmt.Errorf("cannot drop update permission for %s role when table %s on schema %s is untracked", args.Role, tblCfg.name, tblCfg.schema)
				}
				prevElems = append(prevElems, element)
			case *dropDeletePermissionInput:
				if tblCfg.GetState() == "untracked" {
					return fmt.Errorf("cannot drop delete permission for %s role when table %s on schema %s is untracked", args.Role, tblCfg.name, tblCfg.schema)
				}
				prevElems = append(prevElems, element)
			case *addComputedFieldInput:
				if tblCfg.GetState() == "untracked" {
					return fmt.Errorf("cannot add computed field %s when table %s on schema %s is untracked", args.Name, tblCfg.name, tblCfg.schema)
				}
				prevElems = append(prevElems, element)
			case *dropComputedFieldInput:
				if tblCfg.GetState() == "untracked" {
					return fmt.Errorf("cannot drop computed field %s when table %s on schema %s is untracked", args.Name, tblCfg.name, tblCfg.schema)
				}
				prevElems = append(prevElems, element)
			case *setTableCustomFieldsV2Input:
				if tblCfg.GetState() == "untracked" {
					return fmt.Errorf("cannot set custom fields when table %s on schema %s is untracked", tblCfg.name, tblCfg.schema)
				}
				if len(prevElems) != 0 {
					if track, ok := prevElems[0].Value.(*trackTableV2Input); ok {
						track.Configuration = args.tableConfiguration
						squashList.Remove(element)
						continue
					}
				}
				prevElems = append(prevElems, element)
			case *setTableIsEnumInput:
				if tblCfg.GetState() == "untracked" {
					return fmt.Errorf("cannot set table %s on schema %s has a enum when it is untracked", tblCfg.name, tblCfg.schema)
				}
				prevElems = append(prevElems, element)
			}
		}
	}
	return nil
}

func (q CustomQuery) MergeFunctions(squashList *database.CustomList) error {
	functionTransition := transition.New(&functionConfig{})
	functionTransition.Initial("new")
	functionTransition.State("tracked")
	functionTransition.State("untracked")

	functionTransition.Event("track_function").To("tracked").From("new", "untracked")
	functionTransition.Event("untrack_function").To("untracked").From("new", "tracked")

	next := q.Iterate()

	for item, ok := next(); ok; item, ok = next() {
		g := item.(linq.Group)
		if g.Key == nil {
			continue
		}
		funcKey := g.Key.(tableMap)
		funcCfg := functionConfig{
			name:   funcKey.name,
			schema: funcKey.schema,
		}
		prevElems := make([]*list.Element, 0)
		for _, val := range g.Group {
			element := val.(*list.Element)
			switch element.Value.(type) {
			case *trackFunctionInput:
				err := functionTransition.Trigger("track_function", &funcCfg, nil)
				if err != nil {
					return err
				}
				prevElems = append(prevElems, element)
			case *unTrackFunctionInput:
				if funcCfg.GetState() == "tracked" {
					prevElems = append(prevElems, element)
				}
				err := functionTransition.Trigger("untrack_function", &funcCfg, nil)
				if err != nil {
					return err
				}
				// drop previous elements
				for _, e := range prevElems {
					squashList.Remove(e)
				}
			}
		}
	}
	return nil
}

func (q CustomQuery) MergeRemoteSchemas(squashList *database.CustomList) error {
	remoteSchemaTransition := transition.New(&remoteSchemaConfig{})
	remoteSchemaTransition.Initial("new")
	remoteSchemaTransition.State("added")
	remoteSchemaTransition.State("removed")

	remoteSchemaTransition.Event("add_remote_schema").To("added").From("new", "removed")
	remoteSchemaTransition.Event("remove_remote_schema").To("removed").From("new", "added")

	next := q.Iterate()

	for item, ok := next(); ok; item, ok = next() {
		g := item.(linq.Group)
		if g.Key == nil {
			continue
		}
		remoteSchemaKey := g.Key.(string)
		rsCfg := remoteSchemaConfig{
			name: remoteSchemaKey,
		}
		prevElems := make([]*list.Element, 0)
		for _, val := range g.Group {
			element := val.(*list.Element)
			switch element.Value.(type) {
			case *addRemoteSchemaInput:
				err := remoteSchemaTransition.Trigger("add_remote_schema", &rsCfg, nil)
				if err != nil {
					return err
				}
				prevElems = append(prevElems, element)
			case *removeRemoteSchemaInput:
				if rsCfg.GetState() == "added" {
					prevElems = append(prevElems, element)
				}
				err := remoteSchemaTransition.Trigger("remove_remote_schema", &rsCfg, nil)
				if err != nil {
					return err
				}
				// drop previous elements
				for _, e := range prevElems {
					squashList.Remove(e)
				}
			}
		}
	}
	return nil
}

func (q CustomQuery) MergeQueryInCollections(squashList *database.CustomList) error {
	queryInCollectionTransition := transition.New(&queryInCollectionConfig{})
	queryInCollectionTransition.Initial("new")
	queryInCollectionTransition.State("added")
	queryInCollectionTransition.State("dropped")

	queryInCollectionTransition.Event("add_query_to_collection").To("added").From("new", "dropped")
	queryInCollectionTransition.Event("drop_query_from_collection").To("dropped").From("new", "added")

	next := q.Iterate()

	for item, ok := next(); ok; item, ok = next() {
		g := item.(linq.Group)
		if g.Key == nil {
			continue
		}
		queryInCollectionKey := g.Key.(*queryInCollectionMap)
		qicCfg := queryInCollectionConfig{
			collectionName: queryInCollectionKey.collectionName,
			queryName:      queryInCollectionKey.queryName,
		}
		prevElems := make([]*list.Element, 0)
		for _, val := range g.Group {
			element := val.(*list.Element)
			switch element.Value.(type) {
			case *addQueryToCollectionInput:
				err := queryInCollectionTransition.Trigger("add_query_to_collection", &qicCfg, nil)
				if err != nil {
					return err
				}
				prevElems = append(prevElems, element)
			case *dropQueryFromCollectionInput:
				if qicCfg.GetState() == "added" {
					prevElems = append(prevElems, element)
				}
				err := queryInCollectionTransition.Trigger("drop_query_from_collection", &qicCfg, nil)
				if err != nil {
					return err
				}
				// drop previous elements
				for _, e := range prevElems {
					squashList.Remove(e)
				}
			}
		}
	}
	return nil
}

func (q CustomQuery) MergeAllowLists(squashList *database.CustomList) error {
	allowListTransition := transition.New(&allowListConfig{})
	allowListTransition.Initial("new")
	allowListTransition.State("added")
	allowListTransition.State("removed")

	allowListTransition.Event("add_collection_to_allowlist").To("added").From("new", "removed")
	allowListTransition.Event("drop_collection_from_allowlist").To("removed").From("new", "added")

	next := q.Iterate()

	for item, ok := next(); ok; item, ok = next() {
		g := item.(linq.Group)
		if g.Key == nil {
			continue
		}
		allowListKey := g.Key.(string)
		alCfg := allowListConfig{
			collection: allowListKey,
		}
		prevElems := make([]*list.Element, 0)
		for _, val := range g.Group {
			element := val.(*list.Element)
			switch element.Value.(type) {
			case *addRemoteSchemaInput:
				err := allowListTransition.Trigger("add_collection_to_allowlist", &alCfg, nil)
				if err != nil {
					return err
				}
				prevElems = append(prevElems, element)
			case *removeRemoteSchemaInput:
				if alCfg.GetState() == "added" {
					prevElems = append(prevElems, element)
				}
				err := allowListTransition.Trigger("drop_collection_from_allowlist", &alCfg, nil)
				if err != nil {
					return err
				}
				// drop previous elements
				for _, e := range prevElems {
					squashList.Remove(e)
				}
			}
		}
	}
	return nil
}

func (q CustomQuery) MergeQueryCollections(squashList *database.CustomList) error {
	queryCollectionTransition := transition.New(&queryCollectionConfig{})
	queryCollectionTransition.Initial("new")
	queryCollectionTransition.State("created")
	queryCollectionTransition.State("dropped")

	queryCollectionTransition.Event("create_query_collection").To("created").From("new", "dropped")
	queryCollectionTransition.Event("drop_query_collection").To("dropped").From("new", "created")

	next := q.Iterate()

	for item, ok := next(); ok; item, ok = next() {
		g := item.(linq.Group)
		if g.Key == nil {
			continue
		}
		queryCollectionKey := g.Key.(string)
		qcCfg := queryCollectionConfig{
			name: queryCollectionKey,
		}
		prevElems := make([]*list.Element, 0)
		for _, val := range g.Group {
			element := val.(*list.Element)
			switch args := element.Value.(type) {
			case *createQueryCollectionInput:
				err := queryCollectionTransition.Trigger("create_query_collection", &qcCfg, nil)
				if err != nil {
					return err
				}
				prevElems = append(prevElems, element)
			case *addQueryToCollectionInput:
				if qcCfg.GetState() == "dropped" {
					return fmt.Errorf("cannot add query %s to a collection %s when it is dropped", args.QueryName, qcCfg.name)
				}

				if len(prevElems) != 0 {
					if query, ok := prevElems[0].Value.(*createQueryCollectionInput); ok {
						query.Definition.Queries = append(query.Definition.Queries, collectionQuery{
							Name:  args.QueryName,
							Query: args.Query,
						})
						squashList.Remove(element)
						continue
					}
				}
				prevElems = append(prevElems, element)
			case *dropQueryFromCollectionInput:
				prevElems = append(prevElems, element)
			case *addCollectionToAllowListInput:
				if qcCfg.GetState() == "dropped" {
					return fmt.Errorf("cannot add collection %s to allowlist when it is dropped", qcCfg.name)
				}
				qcCfg.allowList = true
				prevElems = append(prevElems, element)
			case *dropCollectionFromAllowListInput:
				qcCfg.allowList = false
				prevElems = append(prevElems, element)
			case *dropQueryCollectionInput:
				if !args.Cascade && qcCfg.allowList {
					// return error stating that without cascade set allow list wont be dropped
					return fmt.Errorf("cannot drop collection %s from allowlist when cascade is not set to true", qcCfg.name)
				}
				if qcCfg.GetState() == "created" {
					prevElems = append(prevElems, element)
				}
				err := queryCollectionTransition.Trigger("drop_query_collection", &qcCfg, nil)
				if err != nil {
					return err
				}
				prevElems = append(prevElems, element)
				// drop previous elements
				for _, e := range prevElems {
					squashList.Remove(e)
				}
			}
		}
	}
	return nil
}

type customList struct {
	*list.List
}

func (c *customList) Iterate() linq.Iterator {
	length := c.Len()
	var prevElem *list.Element
	i := 0
	return func() (item interface{}, ok bool) {
		if length == 0 {
			return
		}

		if i == 0 {
			prevElem = c.Front()
			i++
		} else {
			prevElem = prevElem.Next()
			if prevElem == nil {
				return
			}
		}
		return prevElem, true
	}
}

func (h *HasuraDB) PushToList(migration io.Reader, fileType string, l *database.CustomList) error {
	migr, err := ioutil.ReadAll(migration)
	if err != nil {
		return err
	}
	body := string(migr[:])
	switch fileType {
	case "sql":
		if body == "" {
			break
		}
		tt := &RunSQLInput{
			SQL: body,
		}
		l.PushBack(tt)
	case "meta":
		var t []newHasuraIntefaceQuery
		err := yaml.Unmarshal(migr, &t)
		if err != nil {
			return err
		}
		for _, v := range t {
			switch actionType := v.Args.(type) {
			case *replaceMetadataInput:
				// Remove previous metadata actions
				var next *list.Element
				for e := l.Front(); e != nil; e = next {
					next = e.Next()
					switch e.Value.(type) {
					case *RunSQLInput:
						// do nothing
					default:
						l.Remove(e)
					}
				}
				// add clear Metadata
				cm := &clearMetadataInput{}
				l.PushBack(cm)
				// convert replace metadata to actions
				actionType.convertToMetadataActions(l)
			case *clearMetadataInput:
				// Remove previous metadata actions
				var next *list.Element
				for e := l.Front(); e != nil; e = next {
					next = e.Next()
					switch e.Value.(type) {
					case *RunSQLInput:
						// do nothing
					default:
						l.Remove(e)
					}
				}
				l.PushBack(actionType)
			default:
				l.PushBack(actionType)
			}
		}
	default:
		return fmt.Errorf("Invalid migration file type")
	}
	return nil
}

func (h *HasuraDB) Squash(l *database.CustomList, ret chan<- interface{}) {
	eventTriggersGroup := CustomQuery(linq.FromIterable(l).GroupByT(
		func(element *list.Element) string {
			switch args := element.Value.(type) {
			case *createEventTriggerInput:
				return args.Name
			case *deleteEventTriggerInput:
				return args.Name
			}
			return ""
		}, func(element *list.Element) *list.Element {
			return element
		},
	))
	err := eventTriggersGroup.MergeEventTriggers(l)
	if err != nil {
		ret <- err
	}

	relationshipsGroup := CustomQuery(linq.FromIterable(l).GroupByT(
		func(element *list.Element) interface{} {
			switch args := element.Value.(type) {
			case *createObjectRelationshipInput:
				return relationshipMap{
					args.Table.Name,
					args.Table.Schema,
					args.Name,
				}
			case *createArrayRelationshipInput:
				return relationshipMap{
					args.Table.Name,
					args.Table.Schema,
					args.Name,
				}
			case *setRelationshipCommentInput:
				return relationshipMap{
					args.Table.Name,
					args.Table.Schema,
					args.Name,
				}
			case *dropRelationshipInput:
				return relationshipMap{
					args.Table.Name,
					args.Table.Schema,
					args.RelationShip,
				}
			}
			return nil
		}, func(element *list.Element) *list.Element {
			return element
		},
	))
	err = relationshipsGroup.MergeRelationships(l)
	if err != nil {
		ret <- err
	}

	permissionsGroup := CustomQuery(linq.FromIterable(l).GroupByT(
		func(element *list.Element) interface{} {
			switch args := element.Value.(type) {
			case *createInsertPermissionInput:
				return permissionMap{
					args.Table.Name,
					args.Table.Schema,
					"insert",
					args.Role,
				}
			case *createSelectPermissionInput:
				return permissionMap{
					args.Table.Name,
					args.Table.Schema,
					"select",
					args.Role,
				}
			case *createUpdatePermissionInput:
				return permissionMap{
					args.Table.Name,
					args.Table.Schema,
					"update",
					args.Role,
				}
			case *createDeletePermissionInput:
				return permissionMap{
					args.Table.Name,
					args.Table.Schema,
					"delete",
					args.Role,
				}
			case *dropInsertPermissionInput:
				return permissionMap{
					args.Table.Name,
					args.Table.Schema,
					"insert",
					args.Role,
				}
			case *dropSelectPermissionInput:
				return permissionMap{
					args.Table.Name,
					args.Table.Schema,
					"select",
					args.Role,
				}
			case *dropUpdatePermissionInput:
				return permissionMap{
					args.Table.Name,
					args.Table.Schema,
					"update",
					args.Role,
				}
			case *dropDeletePermissionInput:
				return permissionMap{
					args.Table.Name,
					args.Table.Schema,
					"delete",
					args.Role,
				}
			case *setPermissionCommentInput:
				return permissionMap{
					args.Table.Name,
					args.Table.Schema,
					args.Type,
					args.Role,
				}
			}
			return nil
		}, func(element *list.Element) *list.Element {
			return element
		},
	))
	err = permissionsGroup.MergePermissions(l)
	if err != nil {
		ret <- err
	}

	computedFieldGroup := CustomQuery(linq.FromIterable(l).GroupByT(
		func(element *list.Element) interface{} {
			switch args := element.Value.(type) {
			case *addComputedFieldInput:
				return computedFieldMap{
					args.Table.Name,
					args.Table.Schema,
					args.Name,
				}
			case *dropComputedFieldInput:
				return computedFieldMap{
					args.Table.Name,
					args.Table.Schema,
					args.Name,
				}
			}
			return nil
		}, func(element *list.Element) *list.Element {
			return element
		},
	))
	err = computedFieldGroup.MergeComputedFields(l)
	if err != nil {
		ret <- err
	}

	tableCustomFieldGroup := CustomQuery(linq.FromIterable(l).GroupByT(
		func(element *list.Element) interface{} {
			switch args := element.Value.(type) {
			case *setTableCustomFieldsV2Input:
				return tableMap{
					args.Table.Name,
					args.Table.Schema,
				}
			}
			return nil
		}, func(element *list.Element) *list.Element {
			return element
		},
	))
	err = tableCustomFieldGroup.MergeTableCustomFields(l)
	if err != nil {
		ret <- err
	}

	tableGroups := CustomQuery(linq.FromIterable(l).GroupByT(
		func(element *list.Element) interface{} {
			switch args := element.Value.(type) {
			case *trackTableInput:
				return tableMap{
					args.tableSchema.Name,
					args.tableSchema.Schema,
				}
			case *trackTableV2Input:
				return tableMap{
					args.Table.Name,
					args.Table.Schema,
				}
			case *unTrackTableInput:
				return tableMap{
					args.tableSchema.Name,
					args.tableSchema.Schema,
				}
			case *setTableCustomFieldsV2Input:
				return tableMap{
					args.Table.Name,
					args.Table.Schema,
				}
			case *setTableIsEnumInput:
				return tableMap{
					args.Table.Name,
					args.Table.Schema,
				}
			case *createEventTriggerInput:
				return tableMap{
					args.Table.Name,
					args.Table.Schema,
				}
			case *createObjectRelationshipInput:
				return tableMap{
					args.Table.Name,
					args.Table.Schema,
				}
			case *createArrayRelationshipInput:
				return tableMap{
					args.Table.Name,
					args.Table.Schema,
				}
			case *dropRelationshipInput:
				return tableMap{
					args.Table.Name,
					args.Table.Schema,
				}
			case *createInsertPermissionInput:
				return tableMap{
					args.Table.Name,
					args.Table.Schema,
				}
			case *createSelectPermissionInput:
				return tableMap{
					args.Table.Name,
					args.Table.Schema,
				}
			case *createUpdatePermissionInput:
				return tableMap{
					args.Table.Name,
					args.Table.Schema,
				}
			case *createDeletePermissionInput:
				return tableMap{
					args.Table.Name,
					args.Table.Schema,
				}
			case *dropInsertPermissionInput:
				return tableMap{
					args.Table.Name,
					args.Table.Schema,
				}
			case *dropSelectPermissionInput:
				return tableMap{
					args.Table.Name,
					args.Table.Schema,
				}
			case *dropUpdatePermissionInput:
				return tableMap{
					args.Table.Name,
					args.Table.Schema,
				}
			case *dropDeletePermissionInput:
				return tableMap{
					args.Table.Name,
					args.Table.Schema,
				}
			case *addComputedFieldInput:
				return tableMap{
					args.Table.Name,
					args.Table.Schema,
				}
			case *dropComputedFieldInput:
				return tableMap{
					args.Table.Name,
					args.Table.Schema,
				}
			}
			return nil
		}, func(element *list.Element) *list.Element {
			return element
		},
	))
	err = tableGroups.MergeTables(l)
	if err != nil {
		ret <- err
	}

	functionGroups := CustomQuery(linq.FromIterable(l).GroupByT(
		func(element *list.Element) interface{} {
			switch args := element.Value.(type) {
			case *trackFunctionInput:
				return tableMap{
					args.Name,
					args.Schema,
				}
			case *unTrackFunctionInput:
				return tableMap{
					args.Name,
					args.Schema,
				}
			}
			return nil
		}, func(element *list.Element) *list.Element {
			return element
		},
	))
	err = functionGroups.MergeFunctions(l)
	if err != nil {
		ret <- err
	}

	remoteSchemaGroups := CustomQuery(linq.FromIterable(l).GroupByT(
		func(element *list.Element) string {
			switch args := element.Value.(type) {
			case *addRemoteSchemaInput:
				return args.Name
			case *removeRemoteSchemaInput:
				return args.Name
			}
			return ""
		}, func(element *list.Element) *list.Element {
			return element
		},
	))
	err = remoteSchemaGroups.MergeRemoteSchemas(l)
	if err != nil {
		ret <- err
	}

	allowListGroups := CustomQuery(linq.FromIterable(l).GroupByT(
		func(element *list.Element) string {
			switch args := element.Value.(type) {
			case *addCollectionToAllowListInput:
				return args.Collection
			case *dropCollectionFromAllowListInput:
				return args.Collection
			}
			return ""
		}, func(element *list.Element) *list.Element {
			return element
		},
	))
	err = allowListGroups.MergeAllowLists(l)
	if err != nil {
		ret <- err
	}

	queryInCollectionGroups := CustomQuery(linq.FromIterable(l).GroupByT(
		func(element *list.Element) interface{} {
			switch args := element.Value.(type) {
			case addQueryToCollectionInput:
				return queryInCollectionMap{
					collectionName: args.CollectionName,
					queryName:      args.QueryName,
				}
			case dropQueryFromCollectionInput:
				return queryInCollectionMap{
					collectionName: args.CollectionName,
					queryName:      args.QueryName,
				}
			}
			return nil
		}, func(element *list.Element) *list.Element {
			return element
		},
	))
	err = queryInCollectionGroups.MergeQueryInCollections(l)
	if err != nil {
		ret <- err
	}

	queryCollectionGroups := CustomQuery(linq.FromIterable(l).GroupByT(
		func(element *list.Element) string {
			switch args := element.Value.(type) {
			case *createQueryCollectionInput:
				return args.Name
			case *addQueryToCollectionInput:
				return args.CollectionName
			case *dropQueryFromCollectionInput:
				return args.CollectionName
			case *addCollectionToAllowListInput:
				return args.Collection
			case *dropCollectionFromAllowListInput:
				return args.Collection
			case *dropQueryCollectionInput:
				return args.Collection
			}
			return ""
		}, func(element *list.Element) *list.Element {
			return element
		},
	))
	err = queryCollectionGroups.MergeQueryCollections(l)
	if err != nil {
		ret <- err
	}

	for e := l.Front(); e != nil; e = e.Next() {
		q := HasuraInterfaceQuery{
			Args: e.Value,
		}
		switch args := e.Value.(type) {
		case *trackTableInput:
			q.Type = trackTable
		case *trackTableV2Input:
			q.Version = v2
			q.Type = trackTable
		case *setTableIsEnumInput:
			q.Type = setTableIsEnum
		case *unTrackTableInput:
			q.Type = untrackTable
		case *setTableCustomFieldsV2Input:
			q.Version = v2
			q.Type = setTableCustomFields
		case *createObjectRelationshipInput:
			q.Type = createObjectRelationship
		case *createArrayRelationshipInput:
			q.Type = createArrayRelationship
		case *setRelationshipCommentInput:
			q.Type = setRelationshipComment
		case *dropRelationshipInput:
			q.Type = dropRelationship
		case *createInsertPermissionInput:
			q.Type = createInsertPermission
		case *dropInsertPermissionInput:
			q.Type = dropInsertPermission
		case *createSelectPermissionInput:
			q.Type = createSelectPermission
		case *dropSelectPermissionInput:
			q.Type = dropSelectPermission
		case *createUpdatePermissionInput:
			q.Type = createUpdatePermission
		case *dropUpdatePermissionInput:
			q.Type = dropUpdatePermission
		case *createDeletePermissionInput:
			q.Type = createDeletePermission
		case *dropDeletePermissionInput:
			q.Type = dropDeletePermission
		case *setPermissionCommentInput:
			q.Type = setPermissionComment
		case *trackFunctionInput:
			q.Type = trackFunction
		case *unTrackFunctionInput:
			q.Type = unTrackFunction
		case *createEventTriggerInput:
			q.Type = createEventTrigger
		case *deleteEventTriggerInput:
			q.Type = deleteEventTrigger
		case *addRemoteSchemaInput:
			q.Type = addRemoteSchema
		case *removeRemoteSchemaInput:
			q.Type = removeRemoteSchema
		case *createQueryCollectionInput:
			q.Type = createQueryCollection
		case *dropQueryCollectionInput:
			q.Type = dropQueryCollection
		case *addQueryToCollectionInput:
			q.Type = addQueryToCollection
		case *dropQueryFromCollectionInput:
			q.Type = dropQueryFromCollection
		case *addCollectionToAllowListInput:
			q.Type = addCollectionToAllowList
		case *dropCollectionFromAllowListInput:
			q.Type = dropCollectionFromAllowList
		case *clearMetadataInput:
			q.Type = clearMetadata
		case *addComputedFieldInput:
			q.Type = addComputedField
		case *dropComputedFieldInput:
			q.Type = dropComputedField
		case *RunSQLInput:
			ret <- []byte(args.SQL)
			continue
		default:
			ret <- fmt.Errorf("invalid metadata action")
			return
		}
		ret <- q
	}
}
