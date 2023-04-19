# Skyscope


A tool for visualising and exploring Bazel [Skyframe](https://bazel.build/reference/skyframe) graphs.

![demo](https://github.com/tweag/skyscope/blob/31854a93d861acf404016607d5b70f308e6ae89b/img/demo.gif)

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

As a prerequisite you will need the `graphviz`, `curl` and `jq` packages
installed and `PATH` set so they can be found. Use your preferred package
manager to do this, e.g.  for Ubuntu:

```bash
apt install graphviz
```
Or for MacOS:
```bash
brew install graphviz
```

There are a few different ways to install and run Skyscope itself. Pick
whichever suits best.

### Add it to your `WORKSPACE` file

This method is recommended if you want to quickly try Skyscope with minimal
hassle. Simply add the following to your `WORKSPACE` file:

```python
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
http_archive(
    name = "skyscope",
    sha256 = "b22e68b65330a3666d0e97f859b06df976ee0e167082c1da84865a7d423bb7b5",
    urls = ["https://github.com/tweag/skyscope/releases/download/v0.2.4/skyscope.zip"]
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
[v0.2.4](https://github.com/tweag/skyscope/releases/v0.2.4).

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

![usage-1](https://github.com/tweag/skyscope/blob/31854a93d861acf404016607d5b70f308e6ae89b/img/usage-1.png)

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

![usage-2](https://github.com/tweag/skyscope/blob/31854a93d861acf404016607d5b70f308e6ae89b/img/usage-2.png)

When a node has been expanded all its edges are displayed, including edges
connected to hidden nodes. Hidden nodes are represented by small unlabelled
circles. You can click on these circles to make the hidden nodes visible. To
make all the hidden neighbours of an expanded node visible at once, double
click on it (this may take several seconds for a highly connected node).

### Automatic path finding

If one connected component of the graph depends on another, the dependency path
will be represented by a dotted edge between the components. You can make the
nodes on the dependency path visible by clicking _Open_:

![usage-3](https://github.com/tweag/skyscope/blob/31854a93d861acf404016607d5b70f308e6ae89b/img/usage-3.png)

This feature can be used to discover how a particular target depends on
another, or how an action depends on a file. It works much like a `somepath`
Bazel query.

![usage-4](https://github.com/tweag/skyscope/blob/31854a93d861acf404016607d5b70f308e6ae89b/img/usage-4.png)

### Hiding visible nodes

Collapsing nodes can help keep the size of the graph manageable, but it will
still grow too complex from time to time. When this happens you can crop the
graph to a smaller selection of nodes:

![usage-5](https://github.com/tweag/skyscope/blob/31854a93d861acf404016607d5b70f308e6ae89b/img/usage-5.png)

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

When you run `skyscope import` the Skyscope server is automatically started (or
restarted if it was already running). If you wish to start the server without
importing a new graph, just run `skyscope server`[^1].

### Extra context for targets and actions

The output produced by `bazel dump --skyframe` is sufficient to determine the
Skyframe graph topology, but for many node types it does not provide the full
context. For example, an `ACTION_EXECUTION` node only has a reference to the
`CONFIGURED_TARGET` that created it, a `BUILD_CONFIGURATION`, and an
`actionIndex`:

```
ACTION_EXECUTION:ActionLookupData{
    actionLookupKey=ConfiguredTargetKey{label=//src/main/java/com/google/devtools/build/lib/bazel:BazelServer,
    config=BuildConfigurationValue.Key[29162d16f36425edb5387766d6f9e873585a5b890f0ae3a9e778941f90411445]},
    actionIndex=5
}
```

The `actionIndex` field differentiates this `ACTION_EXECUTION` from others
created by the same `CONFIGURED_TARGET`, but on its own is not very
illuminating. We can use `bazel aquery` to get a list of actions and then
correlate it with `actionIndex` to recover the missing context:

```
action 'Creating runfiles tree bazel-out/k8-fastbuild/bin/src/main/java/com/google/devtools/build/lib/bazel/BazelServer.runfiles'
  Mnemonic: SymlinkTree
  Target: //src/main/java/com/google/devtools/build/lib/bazel:BazelServer
  Configuration: k8-fastbuild
  Execution platform: //:default_host_platform
  ActionKey: 7d501d61ad3623eb44a3f87523c2d681961adf84fb47759e558af0df2d5249d1
  Inputs: [bazel-out/k8-fastbuild/bin/src/main/java/com/google/devtools/build/lib/bazel/BazelServer.runfiles_manifest]
  Outputs: [bazel-out/k8-fastbuild/bin/src/main/java/com/google/devtools/build/lib/bazel/BazelServer.runfiles/MANIFEST]
```

A similar process with `bazel query` provides additional context for
`CONFIGURED_TARGET` nodes. By default Skyscope will attempt to import this
information by running the following when you do `skyscope import`:

```bash
bazel aquery 'deps(//...)'  # Get additional context for action executions
bazel query 'deps(//...)' --output build  # Additional context for targets
```

If either of these commands fail (e.g. because some matching targets are
broken) the import will still continue, but the additional context will be
missing. In this situation you can use the `--aquery` and `--query`
parameters to specify the queries Skyscope should run:[^1]

```bash
skyscope import --query='//src/...' --aquery='//src/main/...'
```

You can also pass `--no-query` and `--no-aquery` to disable importing of
additional context entirely. This is useful when you want to import a Skyframe
graph without affecting it at all (since the `bazel query` and `bazel aquery`
commands can themselves cause nodes to be added to the graph).


### Changing CSS colours and styles

The default theme is embedded in the Skyscope binary, but you can make it use a
local file instead by setting the `SKYSCOPE_THEME_CSS` environment variable
(probably in `~/.bashrc` or equivalent). Note that if a Skyscope server is
already running, it will not pick up the new environment variable until it is
restarted (`pkill -x skyscope`).

It is recommended that you begin by making a local copy of the [default
theme](https://github.com/tweag/skyscope/blob/master/frontend/src/theme.css)
and edit that as needed. Any changes you make can be checked immediately by
refreshing your browser. The relevant section for colours is this:

```css
div.ResultRow.ActionExecution           span.NodeTitle { color: hsl(318, 55%, 29%); }
div.ResultRow.ConfiguredTarget          span.NodeTitle { color: hsl(117, 55%, 29%); }
div.ResultRow.FileState                 span.NodeTitle { color: hsl(271, 55%, 29%); }

g.node.ActionExecution                  text.NodeTitle {  fill: hsl(318, 55%, 29%); }
g.node.ConfiguredTarget                 text.NodeTitle {  fill: hsl(117, 55%, 29%); }
g.node.FileState                        text.NodeTitle {  fill: hsl(271, 55%, 29%); }
```

Set `color` for `div.ResultRow` selectors to change the colour of a particular
node type in the search box results. The `g.node` selectors set the `fill`
colour of the node title in the graph. While `div.ResultRow.Foo` can be set
independently from `g.node.Foo`, for consistency they should be the same.

### Changing node label formatting

The majority of the frontend is written in Purescript, but the formatting of
node labels is done in Javascript to allow easy customisation. If you wish to
change this formatting, you can make a local copy of the [default format
file](https://github.com/tweag/skyscope/blob/master/frontend/src/format.js) and
set the `SKYSCOPE_FORMAT_JS` environment variable appropriately. For reference,
a minimal `format.js` would be:

```javascript
// You have access to a `node` object with the following fields:
//   node.type        Camelcase version of SkyKey.functionName()
//   node.data        Raw SkyKey as printed by bazel dump
//   node.label       First label extracted from node data (possibly empty)
//   node.context     Extra context, if there is any (e.g. from query or aquery)

const title = ""  // Use default (which is node.type)

const detail = ""  // Use default (which is node.label if non-empty, or node.data otherwise)

const tooltip = ""  // Use default (which is node.data)

return { title, detail, tooltip };
```

Returning an empty string for one of the fields will cause the default
formatting to be used for that field. The four input fields can be parsed and
combined as you wish to produce the output fields. You might want to add a
temporary `console.log(node)` statement to see what the input fields look like
in your browser console.

### Environment variable glossary


| Variable Name        | Description                                                                           |
|----------------------|---------------------------------------------------------------------------------------|
| `SKYSCOPE_PORT`      | Port on which the Skyscope HTTP server should listen. Defaults to `28581`.            |
| `SKYSCOPE_DATA`      | Directory where imports, server pid file, and other data should be stored. Defaults to `$HOME/.skyscope` if not set. |
| `SKYSCOPE_DEBUG`     | Execution tracing is enabled for the wrapper scripts when this variable is set. |
| `SKYSCOPE_FORMAT_JS` | Set this to the path of a [Javascript file](https://github.com/tweag/skyscope/blob/master/frontend/src/format.js) to override the embedded node formatting. |
| `SKYSCOPE_THEME_CSS` | Set this to the path of a [CSS file](https://github.com/tweag/skyscope/blob/master/frontend/src/theme.css) to override the embedded theme. |

## Architectural Overview

The backend is split into separate server and import processes. This allows a
single server process to be used across multiple workspaces. The import process
extracts the Skyframe data from Bazel and inserts it into an Sqlite database.
After this is done it notifies the server of the newly imported graph. The
server keeps track of all imported graphs in a central Sqlite database.

![architecture](https://github.com/tweag/skyscope/blob/31854a93d861acf404016607d5b70f308e6ae89b/img/architecture.svg)

The frontend is responsible for storing the node configuration (i.e. which
nodes are visible) and whenever this changes it sends a request to the backend
to rerender the graph ([Graphviz](https://graphviz.org/) is used for this).
The response is an unstyled SVG image which the frontend then decorates with
CSS and various event handlers.

## Contributing

You can contribute by reporting bugs or requesting features you would like to
see in the [GitHub issue tracker](https://github.com/tweag/skyscope/issues).
Pull requests are also welcome, but for non-trivial changes please discuss the
change you have in mind first so we can agree on an approach. If there is an
existing issue you can comment on that, otherwise you can open a new issue.

[^1]: In this case, you should take subsequent references to `skyscope COMMAND
ARGS` to mean `bazel run -- @skyscope//:COMMAND ARGS`.

[^2]: Usually _Alt + Left Arrow_ to go back and _Alt + Right Arrow_ to go
forward.
