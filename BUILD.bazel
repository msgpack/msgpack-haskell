load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_library")

haskell_library(
    name = "hs-msgpack",
    srcs = glob(["src/**/*.*hs"]),
    prebuilt_dependencies = [
        "base",
        "binary",
        "bytestring",
    ],
    src_strip_prefix = "src",
    visibility = ["//visibility:public"],
    deps = [
        "//hs-msgpack-types",
        "@haskell_data_binary_ieee754//:data-binary-ieee754",
        "@haskell_text//:text",
    ],
)
