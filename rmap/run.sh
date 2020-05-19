docker run -d -p 3838:3838 \
  -v ${PWD}/:/srv/shiny-server/ \
  --name shinyapp \
  shinyapp
