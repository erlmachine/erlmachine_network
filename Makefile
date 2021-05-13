PROJECT = erlmachine_network
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = erlmachine gun

dep_erlmachine = git https://github.com/Erlmachine/erlmachine
dep_gun = git https://github.com/ninenines/gun

include erlang.mk
