const truncate = (content, n) => {
    const truncated = content.slice(0, n);
    const ellipsis = truncated == content ? "" : "…";
    return truncated + ellipsis;
};

const title = function () {
    var match = null;
    switch (node.type) {
        case "ConfiguredTarget":
        case "TransitiveTarget":
        case "TargetCompletion":
            match = node.context.match("(?<=\n)\\w+(?=\\(\n)");
            if (match != null) {
                return truncate(match[0], 25);
            }
            break;
        case "ActionExecution":
            match = node.context.match("(?<=Mnemonic: )\\w+");
            if (match != null) {
                return truncate(match[0], 25);
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
        case "ActionEnvironmentVariable":
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
