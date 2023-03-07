# Skyscope

A tool for visualising and exploring Bazel [Skyframe](https://bazel.build/reference/skyframe) graphs.

![skyscope-demo](https://github.com/tweag/skyscope/blob/c27f550ed71e50841d19656ca7dc6c89b9e7217d/img/skyscope-demo.gif)

## Usage

### Importing a graph

```bash
# Clone this repository
git clone https://github.com/tweag/skyscope.git

# Add the wrapper to your PATH variable
export PATH="$PWD/skyscope/bin:$PATH"

# Run a bazel command and import the graph into Skyscope
cd some-project && bazel build //some:target && skyscope
```

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
