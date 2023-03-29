def _entrypoint_impl(ctx):
    output = ctx.actions.declare_file(ctx.attr.name + ".sh")
    default_info = ctx.attr.binary[DefaultInfo]
    binary = default_info.files.to_list()[0]
    ctx.actions.expand_template(
        template = ctx.file.src,
        output = output,
        substitutions = {
            "#entrypoint": """
                export RUNFILES="$PWD"
                cd "${{SKYSCOPE_WORKSPACE:-$BUILD_WORKSPACE_DIRECTORY}}"
                export SKYSCOPE_WORKSPACE="$(bazel info workspace)"
                export SKYSCOPE_OUTPUT_BASE="$(bazel info output_base)"
                export SKYSCOPE_DATA="${{SKYSCOPE_DATA:-$HOME/.skyscope}}"
                export BAZEL_VERSION="$(bazel version | grep -Po '(?<=Build label: ).*')"
                skyscope() {{
                    "$RUNFILES/{binary}" +RTS -N -RTS "$@"
                }}
                {includes}
            """.format(
                binary = binary.short_path,
                includes = "\n".join([
                    "source \"$RUNFILES/{}\"".format(file.short_path)
                    for file in ctx.files.includes
                ]),
            ),
        },
    )
    runfiles = ctx.runfiles([binary] + ctx.files.includes)
    return [DefaultInfo(
        executable = output,
        runfiles = runfiles.merge(default_info.default_runfiles),
    )]

entrypoint = rule(
    implementation = _entrypoint_impl,
    executable = True,
    attrs = {
        "src": attr.label(
            allow_single_file = True,
            mandatory = True,
        ),
        "binary": attr.label(
            executable = True,
            mandatory = True,
            cfg = "exec",
        ),
        "includes": attr.label_list(
            allow_files = [".sh"],
        ),
    },
)

def _ld_binary_impl(ctx):
    inputs = [ctx.file.binary] + ctx.files.libraries
    output = ctx.actions.declare_file(ctx.attr.name)
    wrapper = """
        RUNFILES="${{RUNFILES:-$PWD}}"
        LD_LIBRARY_PATH="$RUNFILES/{lib_paths}" "$RUNFILES/{binary}" "$@"
    """
    lib_paths = ":".join(depset([
        lib.dirname
        for lib in ctx.files.libraries
    ]).to_list())
    ctx.actions.write(
        output,
        wrapper.format(
            lib_paths = lib_paths,
            binary = ctx.file.binary.path,
        ),
        is_executable = True,
    )
    return [DefaultInfo(
        executable = output,
        runfiles = ctx.runfiles(inputs),
    )]

ld_binary = rule(
    implementation = _ld_binary_impl,
    executable = True,
    attrs = {
        "binary": attr.label(
            allow_single_file = True,
            mandatory = True,
        ),
        "libraries": attr.label_list(
            allow_files = True,
        ),
    },
)
