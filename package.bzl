def _package_binary_impl(ctx):
    output = ctx.actions.declare_file(ctx.attr.name)
    ctx.actions.run_shell(
        outputs = [output],
        inputs = [
            ctx.file.binary,
            ctx.file.entrypoint,
        ],
        tools = [
            ctx.file._ldd,
            ctx.file._makeself,
            ctx.file._header,
        ],
        command = """
            set -eux
            PACKAGE_DIR="package/{name}"
            mkdir -p "$PACKAGE_DIR"
            DEPS=$("{ldd}" "$(realpath "{binary}")" | grep -Po '(?<= => )\\S+')
            cp "{binary}" "{entrypoint}" $DEPS "$PACKAGE_DIR"
            HEADER="package/$(basename "{header}")"
            cp "{header}" "$HEADER" # Modify header to forward all arguments to skyscope, disabling makeself args.
            sed -i 's/^while true$/if test "$1" = --phase2\\nthen copy=phase2; shift\\nfi\\nwhile false/' "$HEADER"
            "{makeself}" --header "$HEADER" "$PACKAGE_DIR" "{output}" "{description}" "./$(basename "{entrypoint}")"
        """.format(
            name = ctx.attr.name,
            description = ctx.attr.description,
            binary = ctx.file.binary.path,
            entrypoint = ctx.file.entrypoint.path,
            ldd = ctx.file._ldd.path,
            makeself = ctx.file._makeself.path,
            header = ctx.file._header.path,
            output = output.path,
        ),
    )
    return [DefaultInfo(files = depset([output]))]

package_binary = rule(
    implementation = _package_binary_impl,
    attrs = {
        "binary": attr.label(
            allow_single_file = True,
            mandatory = True,
        ),
        "entrypoint": attr.label(
            allow_single_file = True,
            mandatory = True,
        ),
        "description": attr.string(),
        "_ldd": attr.label(
            allow_single_file = True,
            default = "@glibc.bin//:bin/ldd",
        ),
        "_makeself": attr.label(
            allow_single_file = True,
            default = "@makeself//:bin/makeself",
        ),
        "_header": attr.label(
            allow_single_file = True,
            default = "@makeself//:share/makeself-2.4.5/makeself-header.sh",
        ),
    },
)
