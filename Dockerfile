FROM torus/violet:latest

RUN apt-get install -y jq

WORKDIR /code
CMD make run
