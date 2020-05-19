# COVID DASHBOARD ORCHESTRATOR
NGINX as reverse proxy to /api and /map endpoints.

## INSTALLATION
1. install `docker-engine` and `docker-compose`, following instructions on docker
   website.
2. clone this repo: `git clone https://github.com/geoboom/covid-dashboard-docker.git`
3. `cd covid-dashboard-docker`
4. `git submodule init && git submodule update` to update the frontend and
   backend submodules.
5. `docker-compose up -d` to build the docker containers and up them.

