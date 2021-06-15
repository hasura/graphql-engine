FROM mcr.microsoft.com/mssql/server:2019-CU10-ubuntu-20.04

ENV ACCEPT_EULA=Y
ENV SA_PASSWORD=${MSSQL_PASSWORD:-hasuraMSSQL1}

USER root
RUN apt-get update && \
    apt-get install -y mssql-tools && \
    ln -sfn /opt/mssql-tools/bin/sqlcmd /usr/local/bin/sqlcmd
