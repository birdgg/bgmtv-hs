# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Haskell library project named `bgm-tv` using Cabal as the build system. The project follows standard Haskell project structure with GHC2024 as the default language extension.

## Build System & Common Commands

This project uses Cabal for dependency management and building:

- **Build the project**: `cabal build`
- **Run tests**: `cabal test`
- **Install dependencies**: `cabal update && cabal build --dependencies-only`
- **Clean build artifacts**: `cabal clean`
- **Start REPL with project loaded**: `cabal repl`

## Project Structure

- `src/`: Source directory containing library modules
- `bgm-tv.cabal`: Cabal package configuration with build settings and dependencies
- The main exposed module is `MyLib` (as defined in the cabal file)

## Development Notes

- The project uses GHC2024 language extensions by default
- Compiler warnings are enabled via `-Wall` flag
- Base dependency is pinned to `^>=4.21.0.0`
- License: MIT

## Current State

The project appears to be a fresh Haskell project template - no source files have been created yet, only the basic Cabal configuration exists.