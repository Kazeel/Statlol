###############################
# Shiny Server
##############################

library(rsconnect)

rsconnect::setAccountInfo(name='kazeel',
                          token='C56F06DDB4D26E5A6C5F99E90E5C8722',
                          secret='XXcufo3M/ShNcVkugapXJhWAK6Ig5q96eNWgNybg')


rsconnect::deployApp()
