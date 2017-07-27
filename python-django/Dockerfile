FROM python:3.5.2-alpine

WORKDIR /usr/src/app

# install requirements
# this way when you build you won't need to install again
# and since COPY is cached we don't need to wait
COPY app/conf/requirements.txt /tmp/requirements.txt
RUN pip3 install -r /tmp/requirements.txt

# Copy the Configuration
COPY app/conf /conf

# Copy the source
COPY app/src /usr/src/app

#CMD ["gunicorn", "-w 4", "-b 0.0.0.0:8080", "helloworld.wsgi"]

# App port number is configured through the gunicorn config file
CMD ["gunicorn", "--config", "/conf/gunicorn_config.py", "helloworld.wsgi"]
