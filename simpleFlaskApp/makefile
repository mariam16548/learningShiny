all:
    make env && \
    make debug && \
    flask run

env:
    set FLASK_APP=simpleHelloApp.py

debug:
    FLASK_APP=simpleHelloApp.py FLASK_ENV=development flask run
