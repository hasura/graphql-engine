package database

type GraphQLDriver interface {
	GetIntroSpectionSchema() (interface{}, error)
}
