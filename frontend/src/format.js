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
            if (match) {
                return truncate(match[0], 25);
            }
            break;
        case "ActionExecution":
            match = node.context.match("(?<=Mnemonic: )\\w+");
            if (match) {
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
            if (match) {
                return match[1] + "/" + match[2];
            }
            break;
        case "Artifact":
            match = node.data.match(/\[.*\]([^\[\]]+)/);
            if (match) {
                return match[1];
            }
            break;
        case "ActionEnvironmentVariable":
        case "ClientEnvironmentVariable":
        case "ContainingPackageLookup":
        case "IgnoredPackagePrefixes":
        case "Package":
        case "PackageLookup":
        case "Precomputed":
        case "RepositoryDirectory":
            match = node.data.match(/:(.*)/);
            if (match) {
                return match[1];
            }
            break;
        case "Glob":
            match = node.data.match(/subdir=(.*) pattern=(.+) globberOperation/);
            if (match) {
                return match[1] + (match[1].length > 0 ? "/" : "") + match[2];
            }
            break;
        case "BzlLoad":
        case "ToolchainResolution":
        case "SingleToolchainResolution":
            if (node.label) {
                return node.label;
            }
        case "BuildConfiguration":
        case "RegisteredToolchains":
            match = node.data.match(/BuildConfigurationKey\[([0-9a-f]{64})\]/);
            if (match) {
                return match[1].slice(0, 16);
            }
            break;
    }
    return "";
} ();

const tooltip = function () {
    return `${node.context}`;
} ();

return { title, detail, tooltip };
