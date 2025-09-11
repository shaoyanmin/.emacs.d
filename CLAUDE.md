# Emacs Configuration

This is a personal Emacs configuration that emphasizes modern tooling, cross-platform compatibility, and developer productivity.

## Architecture

The configuration uses a modular structure with specialized files:

- `init.el` - Main entry point and orchestration
- `elisp/env.el` - Environment variables and system paths
- `elisp/editor.el` - Core editor settings
- `elisp/setting.el` - General preferences
- `elisp/package-config.el` - Package configurations
- `elisp/style.el` - Visual themes and appearance
- `elisp/keybinding.el` - Custom key mappings

## Key Features

### Modern Completion Framework
- **Vertico** - Fast vertical completion UI
- **Consult** - Enhanced command interface
- **Orderless** - Flexible matching patterns
- **Marginalia** - Rich completion annotations

### Development Tools
- **Eglot** - Language Server Protocol integration
- **Projectile** - Project management
- **Magit** - Git interface
- **Multiple Cursors** - Multi-cursor editing
- **Undo Tree** - Visual undo system

### Language Support
- **Scheme** - Complete REPL integration with custom functions
- **Markdown** - Full editing support
- **Salt** - Configuration management files

### AI Integration
- **Claude Code** - AI pair programming assistant

### Cross-Platform Support
Conditional configurations for Windows, macOS, and Linux with appropriate:
- Font settings (Consolas, Iosevka)
- Path management
- Platform-specific optimizations

## Package Management

Uses dual package management:
- **package.el** - Standard Emacs package manager (MELPA, ELPA, Org)
- **straight.el** - Declarative package management for bleeding-edge packages

## Terminal Integration
- **VTerm** - Fast terminal emulator

## Optional Components
- **PDF Tools** - PDF viewing and annotation (requires manual compilation)
- **PlantUML** - Diagram generation (if jar file present)

## Commands to Know

Common development commands:
- Run linter: Check project-specific files for linting commands
- Type check: Look for type checking setup in project files
- Tests: Examine project structure for testing frameworks

## Testing Configuration

To test if the Emacs configuration loads without errors:

```bash
emacs --batch --eval "(progn (load \"~/.emacs.d/init.el\") (message \"Emacs configuration loaded successfully\"))"
```

This command:
- Runs Emacs in batch mode (no GUI)
- Loads the init.el configuration
- Reports success or shows any loading errors
- Exits automatically

The configuration should load successfully with only minor deprecation warnings.

## Recent Changes

Based on git history:
- Migrated from ivy to vertico/consult/orderless completion
- Disabled ido mode in favor of vertico
- Fixed Linux terminal warnings
- Added recentf-mode integration
- Enhanced Claude Code integration