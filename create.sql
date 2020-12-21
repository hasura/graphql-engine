create table [Artist] (ArtistId int not null, Name nvarchar(max), constraint artist_pk primary key nonclustered (ArtistId))

create table [Album] (AlbumId int not null, Title nvarchar(max), ArtistId int not null, constraint album_pk primary key nonclustered (AlbumId), constraint album_fk foreign key (ArtistId) references Artist(ArtistId))

create table [Track] (TrackId int not null, Title nvarchar(max), ArtistId int not null,AlbumId int not null, constraint track_pk primary key nonclustered (TrackId), constraint track_fk foreign key (ArtistId) references Artist(ArtistId), constraint track_fk2 foreign key (AlbumId) references Album(AlbumId))
