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

def package_release(binary, platforms, url_base):
    assemble_closure(
        name = "platform/closure",
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
    wrapper = lambda name: [
        "[[ \"${SKYSCOPE_DEBUG:-}\" ]] && set -x",
        "RELEASE_DIR=\"$PWD/$(find . -type d -name skyscope | head -1)\"",
        "export SKYSCOPE_BINARY=\"$RELEASE_DIR/closure/skyscope\"",
        "\"$RELEASE_DIR/bin/skyscope\" {} \"$@\"".format(name),
    ]
    write_file("platform_import", "platform/import.sh", content = wrapper("import"))
    write_file("platform_server", "platform/server.sh", content = wrapper("server"))
    copy_file("platform_entrypoint", "entrypoint.sh", "platform/bin/skyscope")
    copy_file("platform_BUILD", "platform.BUILD.bazel", "platform/BUILD")
    write_file("platform_WORKSPACE", "platform/WORKSPACE", content = [
        "workspace(name = \"skyscope\")\n",
    ])
    pkg_zip(
        name = "platform-archive",
        out = "skyscope-$PLATFORM.zip",
        strip_prefix = "/release/platform",
        package_dir = "/skyscope",
        srcs = [
            ":platform/BUILD",
            ":platform/WORKSPACE",
            ":platform/closure",
            ":platform/import.sh",
            ":platform/server.sh",
            ":platform/bin/skyscope",
        ],
    )

    for_platforms = lambda str, **kwargs: [
        str.format(
            platform = Label(os).name,
            sha256 = sha256,
            **kwargs
        )
        for os, sha256 in platforms.items()
    ]
    write_file(
        "alias/repository",
        "alias/repository.bzl",
        content = [
            "load(\"@bazel_tools//tools/build_defs/repo:http.bzl\", \"http_archive\")",
            "def configure_skyscope():",
        ] + for_platforms("""
            http_archive(
                name = "skyscope_{platform}",
                urls = ["{url_base}/skyscope-{platform}.zip"],
                sha256="{sha256}",
            )
        """, url_base = url_base),
    )
    make_alias = lambda name: "\n".join([
        "alias(name = \"{}\", actual = select({{".format(name),
        ",".join(for_platforms(
            "\"@platforms//os:{platform}\": \"@skyscope_{platform}//skyscope:{name}\"",
            name = name,
        )),
        "}))",
    ])
    write_file("alias_BUILD", "alias/BUILD", content = [make_alias("import"), make_alias("server")])
    write_file("alias_WORKSPACE", "alias/WORKSPACE", content = ["workspace(name = \"skyscope\")\n"])
    pkg_zip(
        name = "alias-archive",
        out = "skyscope.zip",
        strip_prefix = "/release/alias",
        srcs = [
            ":alias/BUILD",
            ":alias/WORKSPACE",
            ":alias/repository.bzl",
        ],
    )

    #copy_file(
    #    "skyscope-linux",
    #    "skyscope-$PLATFORM.zip",
    #    "skyscope-linux.zip",
    #)
    #copy_file(
    #    "skyscope-macos",
    #    "skyscope-$PLATFORM.zip",
    #    "skyscope-macos.zip",
    #)
