FROM quay.io/centos/centos:stream9
RUN dnf install -y epel-release
RUN dnf install -y pl python3-pip
RUN pip install poetry
WORKDIR /src
# this is ugly
COPY . .
RUN poetry config virtualenvs.create false
RUN poetry install --only main
