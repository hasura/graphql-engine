# passwords/ports are set in docker-compose/databases.yaml

.PHONY: repl-sqlserver
## repl-sqlserver: start a sqlserver docker image and connect to it using sqlcmd
repl-sqlserver:
	@docker compose up -d --wait sqlserver
	@sqlcmd -S localhost,$(shell docker compose port sqlserver 1433 | sed -e 's#.*:\(\)#\1#') -U SA -P "Password!"

.PHONY: repl-postgres
## repl-postgres: start a postgres docker image and connect to it using psql
repl-postgres:
	@docker compose up -d --wait postgres;
	@PORT=$(shell docker compose port postgres 5432 | sed s/.*://); \
		CONNECT_STRING="postgresql://hasura:hasura@localhost:$${PORT}/hasura"; \
		psql $${CONNECT_STRING}
