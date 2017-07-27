# Quickstart - Build your own Docker image#

Build the Docker image using the following command

```bash
$ docker build -t python-django:<tag> .
```

Run the Docker container using the command below.

```bash
$ docker run -d -p 8080:8080 python-django:<tag>
```

# Quickstart - git based pipeline

Follow the steps mentioned below for git based pipeline

1. Ensure that you have a git project
2. Edit `app/src/helloworld/views.py` for controllers. `app/src/helloworld/urls.py` for routes
3. Commit your changes

    ```bash
    $ git add .
    $ git commit -m "message"
    ```

4. Push the changes to git

    ```bash
    $ git push <remote> master
    ```

# Advanced usage

### **Port**

Default Port for application is `8080` .

Application port can be changed by modifying the variable `bind` in  `app/conf/gunicorn_config.py` or setting Environment Variable

```python
bind = "0.0.0.0:" + os.environ.get("APP_PORTS", "<NEW_PORT>")
```

```bash
$ docker run -d -p 8080:<NEW_PORT> python-django:<tag>
```

### **Environment Variables**

* `APP_PORTS` - Application port can also be specified by setting APP_PORTS ENV

  ```bash
  $ docker run -d -p 8080:<NEW_PORT> -e APP_PORTS='<NEW_PORT>' python-django:<tag>
  ```
