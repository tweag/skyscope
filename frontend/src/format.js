const type = function () {
  return `${node.type}`;
} ();

const label = function () {
  var match = null;
  switch (node.type) {
      case "DirectoryListing":
      case "DirectoryListingState":
      case "File":
      case "FileState":
      case "WorkspaceFile":
          match = node.label.match(/.*\[([^\]]*)\]\/\[([^\]]*)\]/);
          if (match != null) {
              return match[1] + "/" + match[2];
          }
          break;
      case "ActionExecution":
      case "ConfiguredTarget":
      case "TargetCompletion":
          match = node.label.match(/label=(.+), config/);
          if (match != null) {
              return match[1];
          }
          break;
      case "Artifact":
          match = node.label.match(/\[.*\]([^\[\]]+)/);
          if (match != null) {
              return match[1];
          }
          break;
      case "BzlLoad":
      case "ClientEnvironmentVariable":
      case "ContainingPackageLookup":
      case "IgnoredPackagePrefixes":
      case "Package":
      case "PackageLookup":
      case "Precomputed":
      case "RepositoryDirectory":
          match = node.label.match(/:(.*)/);
          if (match != null) {
              return match[1];
          }
          break;
      case "Glob":
          match = node.label.match(/subdir=(.*) pattern=(.+) globberOperation/);
          if (match != null) {
              return match[1] + (match[1].length > 0 ? "/" : "") + match[2];
          }
          break;
      case "SingleToolchainResolution":
          match = node.label.match(/toolchainTypeLabel=(.+), targetPlatformKey/);
          if (match != null) {
              return match[1];
          }
          break;
  }
  return node.hash.slice(0, 32);
} ();

const context = function () {
  return `${node.context}`;
} ();

return { type, label, context };
