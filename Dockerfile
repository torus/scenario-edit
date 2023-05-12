FROM torus/violet:latest

RUN apt-get update
RUN apt-get install -y gdb
RUN curl -sL https://deb.nodesource.com/setup_14.x | bash -
RUN apt-get install -y nodejs
RUN apt-get install -y npm
RUN npm install -g nodemon

RUN apt-get install -y jq

RUN apt-get install -y libsqlite3-dev

WORKDIR /code
CMD make run
