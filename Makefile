PROJECT = erlmachine_network
PROJECT_DESCRIPTION = Erlmachine extensions to implement network protocols
PROJECT_VERSION = 1.0.0

DEPS = erlmachine gun

dep_erlmachine = git https://github.com/Erlmachine/erlmachine
dep_gun = git https://github.com/ninenines/gun

include erlang.mk
