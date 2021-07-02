PROJECT = erlmachine_network
PROJECT_DESCRIPTION = Erlmachine extensions to implement network protocols
PROJECT_VERSION = 1.0.0

DEPS = erlmachine gun

TEST_DEPS = erlmachine_eip

dep_erlmachine = git https://github.com/Erlmachine/erlmachine
dep_gun = git https://github.com/ninenines/gun

dep_erlmachine_eip = git https://github.com/Erlmachine/erlmachine_eip.git

include erlang.mk
