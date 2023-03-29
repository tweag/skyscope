# Skyscope


A tool for visualising and exploring Bazel [Skyframe](https://bazel.build/reference/skyframe) graphs.

![skyscope-demo](https://github.com/tweag/skyscope/blob/c27f550ed71e50841d19656ca7dc6c89b9e7217d/img/skyscope-demo.gif)

## Table of Contents
- [Getting Skyscope](#getting-skyscope)  
- [Using Skyscope](#using-skyscope) 
  - [Importing a graph](#importing-the-current-state) 
  - [Searching for nodes](#searching-for-nodes) 
  - [Exploring the graph](#exploring-the-graph) 
  - [Browser history and checkpoints](#browser-history-and-checkpoints) 
  - [Opening paths between components](#opening-paths-between-components) 
  - [Exporting the graph as a static image](#exporting) 
- [Advanced Usage](#advanced-usage) 
  - [Extra context for targets and actions](#extra-context-for-targets-and-actions) 
  - [Changing node colours and other CSS](#theme)
  - [Changing node label formatting](#format)
  - [Environment variable glossary](#environment-variables) 
- [Architectural Overview](#architectural-overview) 
- [Contributing](#contributing) 

## Getting Skyscope

There are a few different ways to install and run Skyscope. Pick whichever suits best.

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

### Build and run it from source

Finally, the git repository contains a [wrapper
script](https://github.com/benradf/skyscope/blob/readme/bin/skyscope) that
invokes Bazel to build and run Skyscope from source. This is useful when making
changes to the Skyscope code. Building takes a little while, the first time it's run.

1. Clone the repository:
```bash
git clone https://github.com/tweag/skyscope.git
```

2. Add the repository `bin` directory to your `PATH` variable; e.g. in `~/.bashrc`:
```bash
export PATH="$HOME/git/skyscope/bin:$PATH"
```

## Using Skyscope

While the API offered to rule implementations requires them to be
[pure](https://en.wikipedia.org/wiki/Pure_function), the Skyframe graph itself
is very much stateful. Most `bazel` commands you can run will have some effect
or another on it.

### Importing a graph

To view a graph you must first import a snapshot of its current state into
Skyscope. If you have `skyscope` in your `PATH` variable, this can be done by
running `skyscope import` under the workspace. If you've added it to your
`WORKSPACE` file, you should instead do `bazel run @skyscope//:import`.[^1]


Depending on the size of the Skyframe graph, the import process might take a
few minutes. Note that the graph is initially empty when the Bazel server
starts and nodes are added as required when you run a Bazel command. So if
importing is taking too long, you can reduce the size of the graph by doing
`bazel shutdown` and then running a command with just the target you are
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

![skyscope-usage-1](https://github.com/tweag/skyscope/blob/c27f550ed71e50841d19656ca7dc6c89b9e7217d/img/skyscope-usage-1.jpg)

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

When a node has been expanded all its edges are displayed, including edges
connected to hidden nodes. In this situation the hidden nodes are represented
by small unlabelled circles. You can click on these circles to make the hidden
nodes visible.

### collapsed and expanded states
### history and local storage restore
### interrupting rendering
### opening paths between components
### exporting svg images


## Advanced Usage

- query and aquery
- environment variables
- customise theme and node formatting


### Importing a graph

```bash
# Clone this repository
git clone https://github.com/tweag/skyscope.git

# Add the wrapper to your PATH variable
export PATH="$PWD/skyscope/bin:$PATH"

# Run a bazel command and import the graph into Skyscope
cd some-project && bazel build //some:target && skyscope
```

The state of the graph depends on what `bazel` commands have been run in the
workspace since the server started. To clear it you can run `bazel shutdown`.

### Exploring the graph

In the following example Skyscope is used to explore the build graph of Bazel itself:

```bash
cd ~/git/bazel && bazel build //src:bazel-dev && skyscope
```

Once the graph has been imported you will be prompted to open http://localhost:28581/ in your browser.

Initially all nodes are hidden so you must search for some to display.
You can toggle the visibility of a node by clicking on it in the search results and the graph will be immediately updated.
The nodes displayed in the search results are a random sample of all the nodes matching the search pattern,
so if there are too many results try a more specific pattern. Use `%` as a wildcard and `_` to match any single character.

![skyscope-usage-1](https://github.com/tweag/skyscope/blob/c27f550ed71e50841d19656ca7dc6c89b9e7217d/img/skyscope-usage-1.jpg)

Press the escape key to close the search box. You can then explore and manipulate the graph directly.

By default nodes are in the _expanded_ state.
This means all their edges are displayed, including edges to nodes that are currently hidden.
In this situation the hidden nodes are represented by small unlabelled circles.
You can click on these circles to make the hidden nodes visible.

Visible nodes may be toggled between the _expanded_ and _collapsed_ states by clicking on them.
When a node has been collapsed, only edges to visible nodes are displayed.
This is useful for certain "nexus" nodes which have hundreds of edges.
For example, if all BuildConfiguration nodes are expanded the graph rendered is 75,000 pixels wide!

### Path finding

Another useful feature of Skyscope is automatic path finding between disjoint components of the visible graph.
You can see this in action by making the `//src:bazel-dev` TargetCompletion node visible:

![skyscope-usage-2](https://github.com/tweag/skyscope/blob/c27f550ed71e50841d19656ca7dc6c89b9e7217d/img/skyscope-usage-2.jpg)

Skyscope knows that `//src:bazel-dev` depends on `SkyFunction.java` and it represents this with a dotted edge between the components.
You can make the nodes on the dependency path visible by clicking _Open path_.
The hidden nodes will be displayed in collapsed mode initially:

![skyscope-usage-3](https://github.com/tweag/skyscope/blob/c27f550ed71e50841d19656ca7dc6c89b9e7217d/img/skyscope-usage-3.jpg)

### Hiding nodes

Collapsing nodes can help keep the size of the graph manageable but it will still get out of control from time to time.
When this happens you can crop the graph to a smaller selection of nodes:

![skyscope-usage-4](https://github.com/tweag/skyscope/blob/c27f550ed71e50841d19656ca7dc6c89b9e7217d/img/skyscope-usage-4.jpg)

To do this, press and hold the shift key while you make your selection.
Upon releasing the shift key, the graph will be updated and only the selected nodes will be visible.
You can also hide individual nodes by ctrl clicking.




<!--


# Importing from Workspaces

To view a Skyframe graph you must first import it into Skyscope.
This can be done by running `skyscope` under the workspace you wish to import from, which depending on the size of the Skyframe graph might take a few minutes.
Note that the graph is initially empty when the Bazel server starts and nodes are added as required when you run a Bazel command.
So if importing is taking too long, you can reduce the size of the graph by doing `bazel shutdown` and then running a command with just the target you are interested in.
When the import process is complete you will be prompted to open a link in your browser.
A list of previously imported graphs can be found at http://localhost:28581 and you can also delete imports from here when they are no longer wanted.


# Searching for Nodes

When you first view a graph all its nodes will be hidden, so you must use the search box to find and display nodes of interest.
The pattern you enter here is matched against node keys, as they are printed by `bazel dump`.
You may use `%` as a wildcard.
As you type the list of results will be dynamically updated and the matching part of each key highlighted.
To keep the interface responsive only a few hundred results will be shown so if you do not see the node you want, try making the search pattern more specific.
Clicking on an entry will toggle its visibility and the graph will be immediately updated underneath the search box.
The search box will stay open until you press escape or click elsewhere.

# Exploring the Graph

    - collapsed and expanded states
    - history and local storage restore
    - interrupting rendering
    - opening paths between components
    - exporting svg images

            By default nodes are in the _expanded_ state.
            This means all their edges are displayed, including edges to nodes that are currently hidden.
            In this situation the hidden nodes are represented by small unlabelled circles.
            You can click on these circles to make the hidden nodes visible.

            Visible nodes may be toggled between the _expanded_ and _collapsed_ states by clicking on them.
            When a node has been collapsed, only edges to visible nodes are displayed.
            This is useful for certain "nexus" nodes which have hundreds of edges.
            For example, if all BuildConfiguration nodes are expanded the graph rendered is 75,000 pixels wide!



By default nodes are in the _collapsed_ state, which means only edges connected to other visible nodes are displayed.
This helps keep the complexity of the graph manageable. {{ StarlarkBuiltins example? }}
Visible nodes may be toggled between the _collapsed_ and _expanded_ states by clicking on them.
When a node has been expanded all its edges are displayed, including edges connected to hidden nodes. 
In this situation the hidden nodes are represented by small unlabelled circles.
You can click on these circles to make the hidden nodes visible.





-->


[^1]: In this case, you should take subsequent references to `skyscope COMMAND ARGS` to mean `bazel run @skyscope//:COMMAND -- ARGS`.
