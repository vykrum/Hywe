# Contributing to Hywe

Thank you for your interest in Hywe! 

## Project Philosophy
Hywe is a **design-led exploration**. It is born from a desire to rethink spatial reasoning through computation, prioritizing the **"logic of movement"** and **topology** over conventional architectural software norms.

**Hywe Syntax is the singular source of truth.** Every contribution must respect the principle that the language itself is the foundation of all spatial generation. Everything in this system depends on the integrity and expressiveness of the syntax.

### A Note on Development
The creator of Hywe is primarily a designer exploring the boundaries of design computation. As such, this project does not always follow strict software development "industry norms".

**We embrace an "amateur" spirit in the best sense of the word—doing things for the love of the craft.** We are learning as we go. If you are a seasoned software engineer, your contributions to code quality, architectural best practices, and performance are highly valued, think of it as a form of **technical mentorship** to help this design vision scale.

## How to Contribute

### 1. Reporting Bugs
- Use the [GitHub Issues](https://github.com/vykrum/Hywe/issues) to report bugs.
- Provide a clear description, steps to reproduce, and any relevant **Hywe syntax** (accessible via the **Node/Code toggle** in the workspace).

### 2. Suggesting Features
- We encourage "logic-driven" feature requests that align with the philosophy of topology-first design.
- Open an issue or start a [GitHub Discussion](https://github.com/vykrum/Hywe/discussions).

### 3. Pull Requests
- Fork the repository and create your branch from `main`.
- Hywe is written in **F#**. We prefer a functional programming style, but don't let that stop you. If you have a great idea but aren't an F# expert, open a PR anyway and we can work through the code together.
- **The Weight of Syntax Changes**: Since Hywe Syntax is our singular source of truth, any changes to the syntax itself (adding new attributes, changing existing ones) are considered **high-impact**. Please open a GitHub Discussion or Issue to debate syntax changes before starting work on a Pull Request.
- If you're adding new Hywe syntax attributes or logic, please try to update the relevant documentation or the `llms-full.txt` reference.

## Technical Stack
- **Language**: F# (.NET 8.0+)
- **Web Framework**: Bolero (Blazor WebAssembly)
- **3D Rendering**: WebGPU (via `WebGPU.fs` and custom shaders)
- **2D Rendering**: SVG

## Development Environment
1. Install the [.NET SDK](https://dotnet.microsoft.com/download).
2. Clone the repo: `git clone https://github.com/vykrum/Hywe.git`
3. Run the project: `dotnet watch run --project src/Hywe.Web`
4. Access the app at `https://localhost:51592` (or `http://localhost:51593`). The ports are defined in `src/Hywe.Web/Properties/launchSettings.json`.

## Code of Conduct
By contributing, you agree to abide by our [Code of Conduct](CODE_OF_CONDUCT.md).
