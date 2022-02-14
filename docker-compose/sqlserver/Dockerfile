FROM mcr.microsoft.com/mssql/server:2019-latest@sha256:a098c9ff6fbb8e1c9608ad7511fa42dba8d22e0d50b48302761717840ccc26af

COPY init.sql /init.sql
COPY run-init.sh /run-init.sh
COPY entrypoint.sh /entrypoint.sh

CMD /bin/bash ./entrypoint.sh

ENV PATH="/opt/mssql-tools/bin:${PATH}"
