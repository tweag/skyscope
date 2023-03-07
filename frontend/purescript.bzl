def _spago_packages_impl(repository_ctx):
    repository_ctx.file("spago.dhall", repository_ctx.read(repository_ctx.attr.config))
    repository_ctx.file("packages.dhall", repository_ctx.read(repository_ctx.attr.package_set))

    install_result = repository_ctx.execute(["bash", "-c", "spago build 1>&2 && find .spago/ output/ -type f"])

    if install_result.return_code != 0:
        fail("spago install: {}".format(install_result.stderr))
    for file_path in install_result.stdout.splitlines():
        file_content = repository_ctx.read(file_path)
        repository_ctx.file(file_path, file_content, legacy_utf8 = False)

    repository_ctx.template("BUILD.bazel", Label("//frontend:spago.BUILD.bazel"))

spago_packages = repository_rule(
    implementation = _spago_packages_impl,
    attrs = {
        "config": attr.label(allow_single_file = [".dhall"]),
        "package_set": attr.label(allow_single_file = [".dhall"]),
    },
)
