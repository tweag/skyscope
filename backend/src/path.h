int32_t c_findPath(
    int32_t origin,
    int32_t destination,
    const int32_t* stepMap,
    int32_t stepMapSize,
    int32_t* buffer,
    int32_t size
);

int32_t c_indexPaths(
    int32_t destination,
    const int32_t* predMap,
    int32_t predMapSize,
    int32_t* stepMap,
    int32_t stepMapSize
);
