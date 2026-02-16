DOTNET_CLI_HOME ?= /tmp/dotnet-home

.PHONY: restore build run test watch clean

restore:
	DOTNET_CLI_HOME=$(DOTNET_CLI_HOME) dotnet restore src/pipes.fs.App/pipes.fs.App.fsproj

build: restore
	DOTNET_CLI_HOME=$(DOTNET_CLI_HOME) dotnet build src/pipes.fs.App/pipes.fs.App.fsproj -c Release

run:
	DOTNET_CLI_HOME=$(DOTNET_CLI_HOME) dotnet run --project src/pipes.fs.App -- --mode ink --flow swirl

test: build
	DOTNET_CLI_HOME=$(DOTNET_CLI_HOME) dotnet restore tests/pipes.fs.Tests/pipes.fs.Tests.fsproj
	DOTNET_CLI_HOME=$(DOTNET_CLI_HOME) dotnet run --project tests/pipes.fs.Tests -c Release

watch:
	DOTNET_CLI_HOME=$(DOTNET_CLI_HOME) dotnet watch --project src/pipes.fs.App run -- --mode ink --flow swirl

clean:
	DOTNET_CLI_HOME=$(DOTNET_CLI_HOME) dotnet clean pipes.fs.sln
