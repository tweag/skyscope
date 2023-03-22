load("@rules_pkg//pkg:zip.bzl", "pkg_zip")
load("@bazel_skylib//rules:copy_file.bzl", "copy_file")
load("@bazel_skylib//rules:write_file.bzl", "write_file")

def _assemble_closure_impl(ctx):
    closure = ctx.actions.declare_directory(ctx.attr.name)
    ctx.actions.run_shell(
        outputs = [closure],
        inputs = [ctx.file.binary],
        tools = ctx.files.tools,
        command = """
            set -eu
            {tools}
            mkdir -p "{closure}"
            BIN="$(realpath "{binary}")"
            NEWBIN="{closure}/$(basename "$BIN")"
            cp "$BIN" "$NEWBIN"
            chmod u+w "$NEWBIN"
            case "$(uname -s)" in
                Darwin)
                    RPATH="$("${{TOOLS[otool]}}" -l "$BIN" | grep -A2 LC_RPATH | grep -Po '(?<=path )\\S+')"
                    "${{TOOLS[otool]}}" -L "$BIN" | grep -Po '(?<=\\t).*\\.dylib(?= )' | while read LIBPATH
                        do RESOLVED="${{LIBPATH/@rpath/$RPATH}}"
                           RESOLVED="${{RESOLVED/@loader_path/$(dirname "$BIN")}}"
                           cp "$RESOLVED" "{closure}" || continue
                           NEWLIBPATH="@executable_path/$(basename "$LIBPATH")"
                           "${{TOOLS[install_name_tool]}}" -change "$LIBPATH" "$NEWLIBPATH" "$NEWBIN"
                        done
                    ;;
                Linux)
                    "${{TOOLS[ldd]}}" "$BIN" | grep -Po '(?<= => )\\S+' | while read LIBPATH
                        do cp "$LIBPATH" "{closure}"
                        done
                    ;;
            esac
        """.format(
            closure = closure.path,
            binary = ctx.file.binary.path,
            tools = "declare -A TOOLS=(" + "\n".join([
                "[{}]='{}'".format(name, tool[DefaultInfo].files.to_list()[0].path)
                for tool, name in ctx.attr.tools.items()
            ]) + ")",
        ),
    )
    return [DefaultInfo(files = depset([closure]))]

assemble_closure = rule(
    implementation = _assemble_closure_impl,
    attrs = {
        "binary": attr.label(
            allow_single_file = True,
            mandatory = True,
        ),
        "tools": attr.label_keyed_string_dict(
            allow_files = True,
            mandatory = True,
        ),
    },
)

def package_release(binary):
    assemble_closure(
        name = "package/closure",
        binary = binary,
        tools = select({
            "@platforms//os:linux": {
                "@glibc.bin//:bin/ldd": "ldd",
            },
            "@platforms//os:macos": {
                "@bintools-unwrapped//:bin/otool": "otool",
                "@clang_11//:bin/install_name_tool": "install_name_tool",
            },
        }),
    )
    copy_file("package/import", "import.sh", "package/import.sh")
    copy_file("package/server", "server.sh", "package/server.sh")
    copy_file("package/loader", "loader.bzl", "package/loader.bzl")
    copy_file("package/BUILD", "release.BUILD.bazel", "package/BUILD.bazel")
    write_file("package/WORKSPACE", "package/WORKSPACE.bazel", content = [
        "workspace(name = \"skyscope\")\n",
    ])
    pkg_zip(
        name = "archive",
        out = "skyscope.zip",
        strip_prefix = "/release/package",
        srcs = [
            ":package/closure",
            ":package/import",
            ":package/server",
            ":package/loader",
            ":package/BUILD",
            ":package/WORKSPACE",
        ],
    )
