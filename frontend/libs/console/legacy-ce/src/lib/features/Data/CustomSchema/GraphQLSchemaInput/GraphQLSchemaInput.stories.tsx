import { ComponentMeta, Story } from '@storybook/react';
import { useState } from 'react';
import {
  GraphQLSchemaInput,
  GraphQLSchemaInputProps,
} from './GraphQLSchemaInput';

export default {
  component: GraphQLSchemaInput,
} as ComponentMeta<typeof GraphQLSchemaInput>;

const graphQLSchema = `
# Main Schema
schema {
	query: Query;
}

scalar Date;

# Simple type to contain all scalar types
type AllTypes {
	# Field Description for String
	testString: String;
	# Field Description for Int
	testInt: Int;
	# Field Description for ID
	testID: ID;
	# Field Description for Boolean
	testBoolean: Boolean;
	# Field Description for Float
	testFloat: Float;
}

interface ISearchable {
    searchPreview: String!;
}

union ProductTypes = Movie | Book;

# Testing enum
enum MovieGenere {
    ACTION
    COMEDY
    THRILLER
    DRAMA
}

# Testing Input
input SearchByGenere {
	before: Date;
	after: Date;
	genere: MovieGenere!;
}

# Testing Interface
type Movie implements ISearchable {
	id: ID!;
	searchPreview: String!;
	rentPrice: Float;
	publishDate: Date;
	genere: MovieGenere;
	cast: [String];
}

# Testing Interface
type Book implements ISearchable {
    id: ID!;
	searchPreview: String!;
	price: Float;
	publishDate: Date;
	authors: [String];
}

type Query {
	testString: String;
	testDate; Date;
	allTypes: AllTypes;
	allProducts: [ProductTypes];

	# searches only movies by genere with sophisticated argument
	searchMovieByGenere(searchObject: SearchByGenere!): [Movie];

	# Searchs all products by text string
	searchProduct(text: String!): [ISearchable];
}

`;

export const Primary: Story<GraphQLSchemaInputProps> = args => {
  const [value, setValue] = useState(graphQLSchema);
  return <GraphQLSchemaInput value={value} onChange={setValue} />;
};
