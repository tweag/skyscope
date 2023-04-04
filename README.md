# Skyscope


A tool for visualising and exploring Bazel [Skyframe](https://bazel.build/reference/skyframe) graphs.

![demo](https://github.com/tweag/skyscope/blob/c016dcdf6d7fd9e525b34ce429107221d68172df/img/demo.gif)

## Table of Contents

<!-- bazel run //:update-readme-toc -->

<!-- BEGIN-TOC -->
- [Getting Skyscope](#getting-skyscope)
  - [Add it to your `WORKSPACE` file](#add-it-to-your-workspace-file)
  - [Manually download and install a release](#manually-download-and-install-a-release)
  - [Build and run it from source](#build-and-run-it-from-source-requires-nix)
- [Using Skyscope](#using-skyscope)
  - [Importing a graph](#importing-a-graph)
  - [Searching for nodes](#searching-for-nodes)
  - [Exploring the graph](#exploring-the-graph)
  - [Automatic path finding](#automatic-path-finding)
  - [Hiding visible nodes](#hiding-visible-nodes)
  - [Browser history and checkpoints](#browser-history-and-checkpoints)
  - [Exporting the graph as a static image](#exporting-the-graph-as-a-static-image)
- [Advanced Usage](#advanced-usage)
  - [Extra context for targets and actions](#extra-context-for-targets-and-actions)
  - [Changing CSS colours and styles](#changing-css-colours-and-styles)
  - [Changing node label formatting](#changing-node-label-formatting)
  - [Environment variable glossary](#environment-variable-glossary)
- [Architectural Overview](#architectural-overview)
- [Contributing](#contributing)
<!-- END-TOC -->

## Getting Skyscope

There are a few different ways to install and run Skyscope. Pick whichever suits best.

_While the repository is private to Tweag, only the [Build and run it
from source](#build-and-run-it-from-source-requires-nix) method is supported._

<strike>

### Add it to your `WORKSPACE` file

This method is recommended if you want to quickly try Skyscope with minimal
hassle. Simply add the following to your `WORKSPACE` file:

```python
http_archive(
    name = "skyscope",
    sha256 = "0000000000000000000000000000000000000000000000000000000000000000",
    urls = ["https://github.com/tweag/skyscope/releases/download/v0.0.0/skyscope.zip"]
)
load("@skyscope//:repository.bzl", "configure_skyscope")
configure_skyscope()
```

You will then be able to invoke Skyscope in that workspace with `bazel run`
(see [Using Skyscope](#using-skyscope) for details).

### Manually download and install a release

This method is a little more complicated than the previous one, but it has the
advantage that Skyscope can be run in any workspace without needing to change
its `WORKSPACE` file.

1. Go to the [releases page](https://github.com/tweag/skyscope/releases) and
pick the version you want. The latest is
[v0.0.0](https://github.com/tweag/skyscope/releases/v0.0.0).

2. Download a zip archive for your Operating System (currently supported are
Linux and MacOS).

3. Extract it somewhere; e.g.
```bash
unzip ~/Downloads/skyscope-linux.zip -d ~/.local/
```

4. Update your `PATH` variable appropriately; e.g. append this to `~/.bashrc`:
```bash
export PATH="$HOME/.local/skyscope/bin:$PATH"
```

</strike>

### Build and run it from source (requires [Nix](https://nixos.org/download.html))

Finally, the git repository contains a [wrapper
script](https://github.com/benradf/skyscope/blob/readme/bin/skyscope) that
invokes Bazel to build and run Skyscope from source. This is useful when making
changes to the Skyscope source code. Building takes a little while, on the first run.

1. Clone the repository:
```bash
git clone https://github.com/tweag/skyscope.git
```

2. Add the repository's `bin` directory to your `PATH` variable; e.g. in `~/.bashrc`:
```bash
export PATH="$HOME/git/skyscope/bin:$PATH"
```

## Using Skyscope

The Skyframe graph is stateful. Most `bazel` commands you can run will have
some effect on it.

### Importing a graph

To view a Skyframe graph you must first import a snapshot of its current state
into Skyscope. If you have `skyscope` in your `PATH` variable, this can be done
by running `skyscope import` under the workspace. If you've added it to your
`WORKSPACE` file, you should instead do `bazel run @skyscope//:import`.[^1]

Depending on the size of the Skyframe graph, the import process might take a
few minutes. Note that the graph is initially empty when the Bazel server
starts and nodes are added as required when you run a Bazel command. So if
importing is taking too long, try doing `bazel shutdown` followed by a minimal
sequence of commands to repopulate only the parts of the graph you are
interested in.

When the import process is complete you will be prompted to open a link in your
browser. A list of previously imported graphs can be found at
http://localhost:28581 and you can also delete imports from here when they are
no longer wanted.

### Searching for nodes

When you first view a newly imported graph, all its nodes will be hidden. So
you must use the search box to find and display nodes of interest. The pattern
you enter here is matched against node keys, as they are printed by `bazel dump
--skyframe`. You may use `%` as a wildcard.

![usage-1](https://github.com/tweag/skyscope/blob/c016dcdf6d7fd9e525b34ce429107221d68172df/img/usage-1.png)

As you type, the list of results will be dynamically updated and the matching
part of each key highlighted. To keep the interface responsive only a few
hundred results are shown; so if you do not see the node you want, try making
the search pattern more specific.

Clicking on an entry will toggle its visibility and the graph will be
immediately updated behind the search box. The search box will stay open until
you press escape or click elsewhere so you can continue adding nodes.

### Exploring the graph

By default nodes are in the _collapsed_ state, which means only edges connected
to other visible nodes are displayed. This helps keep the complexity of the
graph manageable. Visible nodes may be toggled between the _collapsed_ and
_expanded_ states by clicking on them.

![usage-2](https://github.com/tweag/skyscope/blob/c016dcdf6d7fd9e525b34ce429107221d68172df/img/usage-2.png)

When a node has been expanded all its edges are displayed, including edges
connected to hidden nodes. Hidden nodes are represented by small unlabelled
circles. You can click on these circles to make the hidden nodes visible.

### Automatic path finding

If one disconnected component of the graph depends on another, the dependency
path will be represented by a dotted edge between the components. You can make
the nodes on the dependency path visible by clicking _Open_:

![usage-3](https://github.com/tweag/skyscope/blob/c016dcdf6d7fd9e525b34ce429107221d68172df/img/usage-3.png)

This feature can be used to discover how a particular target depends on
another, or how an action depends on a file. It works much like a `somepath`
Bazel query.

![usage-4](https://github.com/tweag/skyscope/blob/c016dcdf6d7fd9e525b34ce429107221d68172df/img/usage-4.png)

### Hiding visible nodes

Collapsing nodes can help keep the size of the graph manageable but it will
still grow too complex from time to time. When this happens you can crop the
graph to a smaller selection of nodes:

![usage-5](https://github.com/tweag/skyscope/blob/c016dcdf6d7fd9e525b34ce429107221d68172df/img/usage-5.png)

To do this, press and hold the shift key while you make your selection. Upon
releasing the shift key, the graph will be updated and only the selected nodes
will be visible. You can also hide individual nodes by ctrl clicking.

### Browser history and checkpoints

Every action you take in exploring the graph is added to your browser history.
So if you make a mistake you can undo it by hitting the back button (the
forward button will allow you to redo an undone action). This feature is also
useful when an action causes a large transformation to the graph. Nodes are
animated into their new positions, so jumping back and forth a few times with
keyboard shortcuts[^2] and watching how the nodes move can help you orientate
yourself in the new layout.

For graphs with a small number of visible nodes, rendering is fast enough to be
nearly seamless. For larger graphs it might take several seconds to finish
rendering an update. If rendering is taking too long, you can click on the
spinning hourglass in the lower right corner to interrupt it.

The graph will then be restored to the last checkpoint. A checkpoint is created
whenever rendering completes successfully. It will also be restored when you
reopen a closed graph tab (e.g. after restarting your browser). Checkpoints are
stored in local storage, so clear that if you want to start afresh.

### Exporting the graph as a static image

Unless Skyscope is in the middle of rendering a graph update, the lower right
corner will contain a save icon. Click it to save a static copy of the graph as
an SVG image in your downloads folder. This image is best viewed in a web
browser (not all image viewers fully support the CSS embedded in it).

## Advanced Usage

### Extra context for targets and actions

### Changing CSS colours and styles

### Changing node label formatting

### Environment variable glossary

## Architectural Overview

## Contributing



[^1]: In this case, you should take [subsequent](#todo) [references](#todo) to `skyscope COMMAND ARGS` to mean `bazel run @skyscope//:COMMAND -- ARGS`.

[^2]: Usually _Alt + Left Arrow_ to go back and _Alt + Right Arrow_ to go forward.
