PROJECT = hello_erlang
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy
dep_cowboy_commit = 2.9.0

DEP_PLUGINS = cowboy

# The DEP_PLUGINS line tells the build system to load the plugins Cowboy 
# provides. These include predefined templates that we will use soon.

# If you do make4 run now, Cowboy will be included in the release and
# started automatically. This is not enough however, as Cowboy doesn't do
# anything by default. We still need to tell Cowboy to listen for
# connections.

include erlang.mk

# To run the app, use "make4 run"