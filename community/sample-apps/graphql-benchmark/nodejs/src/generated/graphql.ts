import type { GraphQLResolveInfo } from 'graphql';
export type Maybe<T> = T | null;
export type InputMaybe<T> = Maybe<T>;
export type Exact<T extends { [key: string]: unknown }> = { [K in keyof T]: T[K] };
export type MakeOptional<T, K extends keyof T> = Omit<T, K> & { [SubKey in K]?: Maybe<T[SubKey]> };
export type MakeMaybe<T, K extends keyof T> = Omit<T, K> & { [SubKey in K]: Maybe<T[SubKey]> };
export type RequireFields<T, K extends keyof T> = Omit<T, K> & { [P in K]-?: NonNullable<T[P]> };
/** All built-in and custom scalars, mapped to their actual values */
export type Scalars = {
  ID: string;
  String: string;
  Boolean: boolean;
  Int: number;
  Float: number;
};

export type Album = {
  __typename?: 'Album';
  AlbumId: Scalars['Int'];
  Artist?: Maybe<Artist>;
  ArtistId: Scalars['Int'];
  Title: Scalars['String'];
  Tracks?: Maybe<Array<Track>>;
};

export type Artist = {
  __typename?: 'Artist';
  Albums?: Maybe<Array<Album>>;
  ArtistId: Scalars['Int'];
  Name?: Maybe<Scalars['String']>;
};

export type Genre = {
  __typename?: 'Genre';
  GenreId: Scalars['Int'];
  Name: Scalars['String'];
};

export type Query = {
  __typename?: 'Query';
  Album: Array<Album>;
  Album_by_pk?: Maybe<Album>;
  Artist: Array<Artist>;
  Artist_by_pk?: Maybe<Artist>;
  Genre: Array<Genre>;
  Genre_by_pk?: Maybe<Genre>;
  Track: Array<Track>;
  Track_by_pk?: Maybe<Track>;
};


export type QueryAlbum_By_PkArgs = {
  AlbumId: Scalars['Int'];
};


export type QueryArtist_By_PkArgs = {
  ArtistId: Scalars['Int'];
};


export type QueryGenre_By_PkArgs = {
  GenreId: Scalars['Int'];
};


export type QueryTrack_By_PkArgs = {
  TrackId: Scalars['Int'];
};

export type Track = {
  __typename?: 'Track';
  Album?: Maybe<Album>;
  AlbumId?: Maybe<Scalars['Int']>;
  Bytes?: Maybe<Scalars['Int']>;
  Composer?: Maybe<Scalars['String']>;
  Genre?: Maybe<Genre>;
  GenreId: Scalars['Int'];
  MediaTypeId: Scalars['Int'];
  Milliseconds: Scalars['Int'];
  Name: Scalars['String'];
  TrackId: Scalars['Int'];
};



export type ResolverTypeWrapper<T> = Promise<T> | T;


export type ResolverWithResolve<TResult, TParent, TContext, TArgs> = {
  resolve: ResolverFn<TResult, TParent, TContext, TArgs>;
};
export type Resolver<TResult, TParent = {}, TContext = {}, TArgs = {}> = ResolverFn<TResult, TParent, TContext, TArgs> | ResolverWithResolve<TResult, TParent, TContext, TArgs>;

export type ResolverFn<TResult, TParent, TContext, TArgs> = (
  parent: TParent,
  args: TArgs,
  context: TContext,
  info: GraphQLResolveInfo
) => Promise<TResult> | TResult;

export type SubscriptionSubscribeFn<TResult, TParent, TContext, TArgs> = (
  parent: TParent,
  args: TArgs,
  context: TContext,
  info: GraphQLResolveInfo
) => AsyncIterable<TResult> | Promise<AsyncIterable<TResult>>;

export type SubscriptionResolveFn<TResult, TParent, TContext, TArgs> = (
  parent: TParent,
  args: TArgs,
  context: TContext,
  info: GraphQLResolveInfo
) => TResult | Promise<TResult>;

export interface SubscriptionSubscriberObject<TResult, TKey extends string, TParent, TContext, TArgs> {
  subscribe: SubscriptionSubscribeFn<{ [key in TKey]: TResult }, TParent, TContext, TArgs>;
  resolve?: SubscriptionResolveFn<TResult, { [key in TKey]: TResult }, TContext, TArgs>;
}

export interface SubscriptionResolverObject<TResult, TParent, TContext, TArgs> {
  subscribe: SubscriptionSubscribeFn<any, TParent, TContext, TArgs>;
  resolve: SubscriptionResolveFn<TResult, any, TContext, TArgs>;
}

export type SubscriptionObject<TResult, TKey extends string, TParent, TContext, TArgs> =
  | SubscriptionSubscriberObject<TResult, TKey, TParent, TContext, TArgs>
  | SubscriptionResolverObject<TResult, TParent, TContext, TArgs>;

export type SubscriptionResolver<TResult, TKey extends string, TParent = {}, TContext = {}, TArgs = {}> =
  | ((...args: any[]) => SubscriptionObject<TResult, TKey, TParent, TContext, TArgs>)
  | SubscriptionObject<TResult, TKey, TParent, TContext, TArgs>;

export type TypeResolveFn<TTypes, TParent = {}, TContext = {}> = (
  parent: TParent,
  context: TContext,
  info: GraphQLResolveInfo
) => Maybe<TTypes> | Promise<Maybe<TTypes>>;

export type IsTypeOfResolverFn<T = {}, TContext = {}> = (obj: T, context: TContext, info: GraphQLResolveInfo) => boolean | Promise<boolean>;

export type NextResolverFn<T> = () => Promise<T>;

export type DirectiveResolverFn<TResult = {}, TParent = {}, TContext = {}, TArgs = {}> = (
  next: NextResolverFn<TResult>,
  parent: TParent,
  args: TArgs,
  context: TContext,
  info: GraphQLResolveInfo
) => TResult | Promise<TResult>;

/** Mapping between all available schema types and the resolvers types */
export type ResolversTypes = {
  Album: ResolverTypeWrapper<Album>;
  Artist: ResolverTypeWrapper<Artist>;
  Boolean: ResolverTypeWrapper<Scalars['Boolean']>;
  Genre: ResolverTypeWrapper<Genre>;
  Int: ResolverTypeWrapper<Scalars['Int']>;
  Query: ResolverTypeWrapper<{}>;
  String: ResolverTypeWrapper<Scalars['String']>;
  Track: ResolverTypeWrapper<Track>;
};

/** Mapping between all available schema types and the resolvers parents */
export type ResolversParentTypes = {
  Album: Album;
  Artist: Artist;
  Boolean: Scalars['Boolean'];
  Genre: Genre;
  Int: Scalars['Int'];
  Query: {};
  String: Scalars['String'];
  Track: Track;
};

export type AlbumResolvers<ContextType = any, ParentType extends ResolversParentTypes['Album'] = ResolversParentTypes['Album']> = {
  AlbumId?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  Artist?: Resolver<Maybe<ResolversTypes['Artist']>, ParentType, ContextType>;
  ArtistId?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  Title?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  Tracks?: Resolver<Maybe<Array<ResolversTypes['Track']>>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type ArtistResolvers<ContextType = any, ParentType extends ResolversParentTypes['Artist'] = ResolversParentTypes['Artist']> = {
  Albums?: Resolver<Maybe<Array<ResolversTypes['Album']>>, ParentType, ContextType>;
  ArtistId?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  Name?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type GenreResolvers<ContextType = any, ParentType extends ResolversParentTypes['Genre'] = ResolversParentTypes['Genre']> = {
  GenreId?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  Name?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type QueryResolvers<ContextType = any, ParentType extends ResolversParentTypes['Query'] = ResolversParentTypes['Query']> = {
  Album?: Resolver<Array<ResolversTypes['Album']>, ParentType, ContextType>;
  Album_by_pk?: Resolver<Maybe<ResolversTypes['Album']>, ParentType, ContextType, RequireFields<QueryAlbum_By_PkArgs, 'AlbumId'>>;
  Artist?: Resolver<Array<ResolversTypes['Artist']>, ParentType, ContextType>;
  Artist_by_pk?: Resolver<Maybe<ResolversTypes['Artist']>, ParentType, ContextType, RequireFields<QueryArtist_By_PkArgs, 'ArtistId'>>;
  Genre?: Resolver<Array<ResolversTypes['Genre']>, ParentType, ContextType>;
  Genre_by_pk?: Resolver<Maybe<ResolversTypes['Genre']>, ParentType, ContextType, RequireFields<QueryGenre_By_PkArgs, 'GenreId'>>;
  Track?: Resolver<Array<ResolversTypes['Track']>, ParentType, ContextType>;
  Track_by_pk?: Resolver<Maybe<ResolversTypes['Track']>, ParentType, ContextType, RequireFields<QueryTrack_By_PkArgs, 'TrackId'>>;
};

export type TrackResolvers<ContextType = any, ParentType extends ResolversParentTypes['Track'] = ResolversParentTypes['Track']> = {
  Album?: Resolver<Maybe<ResolversTypes['Album']>, ParentType, ContextType>;
  AlbumId?: Resolver<Maybe<ResolversTypes['Int']>, ParentType, ContextType>;
  Bytes?: Resolver<Maybe<ResolversTypes['Int']>, ParentType, ContextType>;
  Composer?: Resolver<Maybe<ResolversTypes['String']>, ParentType, ContextType>;
  Genre?: Resolver<Maybe<ResolversTypes['Genre']>, ParentType, ContextType>;
  GenreId?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  MediaTypeId?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  Milliseconds?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  Name?: Resolver<ResolversTypes['String'], ParentType, ContextType>;
  TrackId?: Resolver<ResolversTypes['Int'], ParentType, ContextType>;
  __isTypeOf?: IsTypeOfResolverFn<ParentType, ContextType>;
};

export type Resolvers<ContextType = any> = {
  Album?: AlbumResolvers<ContextType>;
  Artist?: ArtistResolvers<ContextType>;
  Genre?: GenreResolvers<ContextType>;
  Query?: QueryResolvers<ContextType>;
  Track?: TrackResolvers<ContextType>;
};

