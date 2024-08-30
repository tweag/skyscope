def _copy_stable_status(ctx):
    ctx.actions.run_shell(
        inputs = [ctx.info_file],
        outputs = [ctx.outputs.output],
        mnemonic = "CopyStableStatus",
        command = "sed -n '/^STABLE_/p' {INFO_FILE} >{OUTPUT}".format(
            INFO_FILE = ctx.info_file.path,
            OUTPUT = ctx.outputs.output.path,
        ),
    )

copy_stable_status = rule(
    implementation = _copy_stable_status,
    attrs = {
        "output" : attr.output(
            mandatory = True,
        ),
    },
)
