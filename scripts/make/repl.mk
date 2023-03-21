.PHONY: repl-sqlserver
## repl-sqlserver: start a sqlserver docker image and connect to it using sqlcmd
repl-sqlserver:
	docker compose up -d --wait sqlserver-healthcheck
	# password/port are set in docker-compose/databases.yaml
	sqlcmd -S localhost,$(shell docker compose port sqlserver 1433 | sed -e 's#.*:\(\)#\1#') -U SA -P "Password!"

