module KVDB
  ( KVDB,
    startKVDB,
    kvGet,
    kvPut,
  )
where

import GenServer

data KVDB k v = KVDB (Server (KVDBMsg k v))

data KVDBMsg k v
  = Put k v
  | Get k (ReplyChan v)

type KVDBState k v = [(k, (v, [ReplyChan v]))]

-- Written by Claude
startKVDB :: (Ord k) => IO (KVDB k v)
startKVDB = do
  server <- spawn kvdbLoop
  return $ KVDB server
  where
    kvdbLoop :: (Ord k) => Chan (KVDBMsg k v) -> IO ()
    kvdbLoop chan = loop []
      where
        loop state = do
          msg <- receive chan
          case msg of
            Put k v -> do
              case lookup k state of
                Just (_, waiters) -> do
                  -- Reply to all waiting gets
                  mapM_ (`reply` v) waiters
                  -- Store new value with no waiters
                  loop $ updateState k (v, []) state
                Nothing ->
                  loop $ (k, (v, [])) : state

            Get k replyChan -> do
              -- Check if value exists
              case lookup k state of
                Just (v, _) ->
                  -- Value exists, reply immediately
                  do
                    reply replyChan v
                    loop state
                Nothing ->
                  -- Value doesn't exist, add to waiters
                  loop $ updateState k (undefined, [replyChan]) state

updateState :: (Ord k) => k -> (v, [ReplyChan v]) -> KVDBState k v -> KVDBState k v
updateState k v [] = [(k, v)]
updateState k v ((k', v') : rest)
  | k == k' = (k, v) : rest
  | otherwise = (k', v') : updateState k v rest

kvGet :: KVDB k v -> k -> IO v
kvGet (KVDB server) k = requestReply server (Get k)

kvPut :: KVDB k v -> k -> v -> IO ()
kvPut (KVDB server) k v = sendTo server (Put k v)