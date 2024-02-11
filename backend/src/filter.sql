--EXPLAIN QUERY PLAN

--SELECT COUNT(*)

SELECT *


FROM (

    WITH filtered_node(idx, hash, data, type, context_key) AS (
        SELECT idx, hash, data, type, context_key FROM node

        --WHERE data LIKE '%actionIndex=8%'
        WHERE data LIKE '%label=//src/google/protobuf/compiler/java:java, config=BuildConfigurationKey[0e913ec885832ba80eb84357b2922ef6ed6bd55134ad82350d1f632bf75f4fa4]}, actionIndex=%'

    )


    -- Neighbours of 37
    SELECT * FROM (
        filtered_node LEFT JOIN edge
        ON filtered_node.idx = edge.source AND edge.target = 37
        OR filtered_node.idx = edge.target AND edge.source = 37
    )


) AS neighbourhood

LEFT JOIN context

ON neighbourhood.context_key = context.context_key

    --SELECT data || CHAR(10) || COALESCE(context_data, '') AS data FROM

LIMIT 1


--UNION VALUES(999999999)



-- JOIN edge
-- ON COALESCE(filtered_node.idx = edge.source AND edge.target = 37, TRUE)
-- OR COALESCE(filtered_node.idx = edge.target AND edge.source = 37, TRUE)

-- SELECT hash, data, type FROM node WHERE data LIKE ?

        
--        SELECT *
--        
--        FROM (
--            SELECT idx, hash, type, data || CHAR(10) || COALESCE(context_data, '') AS data
--            FROM (
--                node LEFT JOIN edge
--                ON node.idx = edge.source
--                OR node.idx = edge.target
--            ) AS neighbourhood
--            LEFT JOIN context ON node.context_key = context.context_key
--        
--        
--        )
--        AS conjoined_data
--        
--        
--        WHERE type = 'ACTION_EXECUTION'
--        
--        --AND data LIKE '%label=//src/google/protobuf/compiler/java:java, config=BuildConfigurationKey[0e913ec885832ba80eb84357b2922ef6ed6bd55134ad82350d1f632bf75f4fa4]}, actionIndex=8}%'
--        AND data LIKE '%label=//src/google/protobuf/compiler/java:java, config=BuildConfigurationKey[0e913ec885832ba80eb84357b2922ef6ed6bd55134ad82350d1f632bf75f4fa4]}, actionIndex=8}%'
--        -- idx=37
        

;
