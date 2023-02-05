{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Warpless.Internal
  ( -- * Settings
    Settings (..),
    ProxyProtocol (..),

    -- * Low level run functions
    runSettingsConnection,
    runSettingsConnectionMaker,
    runSettingsConnectionMakerSecure,
    Transport (..),

    -- * Connection
    Connection (..),
    socketConnection,

    -- ** Receive
    Recv,
    RecvBuf,
    makePlainReceiveN,

    -- ** Buffer
    Buffer,
    BufSize,
    WriteBuffer (..),
    createWriteBuffer,
    allocateBuffer,
    freeBuffer,
    copy,

    -- ** Sendfile
    FileId (..),
    SendFile,
    sendFile,
    readSendFile,

    -- * Version
    warpVersion,

    -- * Data types
    InternalInfo (..),
    HeaderValue,
    IndexedHeader,
    requestMaxIndex,

    -- * Time out manager

    -- |
    --
    -- In order to provide slowloris protection, Warp provides timeout handlers. We
    -- follow these rules:
    --
    -- * A timeout is created when a connection is opened.
    --
    -- * When all request headers are read, the timeout is tickled.
    --
    -- * Every time at least the slowloris size settings number of bytes of the request
    --   body are read, the timeout is tickled.
    --
    -- * The timeout is paused while executing user code. This will apply to both
    --   the application itself, and a ResponseSource response. The timeout is
    --   resumed as soon as we return from user code.
    --
    -- * Every time data is successfully sent to the client, the timeout is tickled.
    module System.TimeManager,

    -- * File descriptor cache
    module Warpless.FdCache,

    -- * File information cache
    module Warpless.FileInfoCache,

    -- * Date
    module Warpless.Date,

    -- * Request and response
    Source,
    recvRequest,
    sendResponse,

    -- * Platform dependent helper functions
    setSocketCloseOnExec,

    -- * Misc
    http2server,
    withII,
    pReadMaker,
  )
where

import Network.Socket.BufferPool
import System.TimeManager
import Warpless.Buffer
import Warpless.Date
import Warpless.FdCache
import Warpless.FileInfoCache
import Warpless.HTTP2
import Warpless.HTTP2.File
import Warpless.Header
import Warpless.Request
import Warpless.Response
import Warpless.Run
import Warpless.SendFile
import Warpless.Settings
import Warpless.Types
