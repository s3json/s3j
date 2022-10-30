s3j-core-internal
=================

Very basic version of generation macros, which is used to generate s3j internal codecs.

Full-featured macros have dependency both on `s3j-runtime` and `s3j-schema`, so they cannot be used to generate serializers inside these projects.

These macros depend only on `s3j-runtime`, so they could be used to generate serializers in other s3j subprojects.
