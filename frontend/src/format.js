const title = function () {
    var match = null;
    switch (node.type) {
        case "ConfiguredTarget":
        case "TransitiveTarget":
        case "TargetCompletion":
            break;
        case "ActionExecution":
            match = node.context.match("(?<=Mnemonic: )\\w+");
            if (match != null) {
                const truncated = match[0].slice(0, 20);
                const ellipsis = truncated == match[0] ? "" : "…";
                return `Action: ${truncated + ellipsis}`;
            }
            break;
    }
    return "";
} ();

const detail = function () {
    var match = null;
    switch (node.type) {
        case "DirectoryListing":
        case "DirectoryListingState":
        case "File":
        case "FileState":
        case "WorkspaceFile":
            match = node.data.match(/.*\[([^\]]*)\]\/\[([^\]]*)\]/);
            if (match != null) {
                return match[1] + "/" + match[2];
            }
            break;
        case "Artifact":
            match = node.data.match(/\[.*\]([^\[\]]+)/);
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
            match = node.data.match(/:(.*)/);
            if (match != null) {
                return match[1];
            }
            break;
        case "Glob":
            match = node.data.match(/subdir=(.*) pattern=(.+) globberOperation/);
            if (match != null) {
                return match[1] + (match[1].length > 0 ? "/" : "") + match[2];
            }
            break;
        case "SingleToolchainResolution":
            match = node.data.match(/toolchainTypeLabel=(.+), targetPlatformKey/);
            if (match != null) {
                return match[1];
            }
            break;
    }
    return "";
} ();

const tooltip = function () {
    return `${node.context}`;
} ();

return { title, detail, tooltip };
