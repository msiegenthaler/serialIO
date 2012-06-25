module System.Hardware.Serial (
    -- * Open
    PortName,
    openSerialPort,
    withSerialPort,
    -- * Settings
    SerialPortSetting(..),
    defaultSettings,
    BaudRate(..),
    StopBits(..),
    Parity(..),
    FlowControl(..)
) where

import System.IO
import Control.Exception
import System.Hardware.Serial.Settings
import qualified System.Hardware.Serial.Posix as U

-- Name/FilePath of the serial port (i.e. /dev/tty1 on a unix).
type PortName = String

-- | Opens the serial port and returns a handle for it. The port is opened in binary mode with
--   no buffering, but these settings can be changed directly on the handle.
openSerialPort :: PortName -> SerialPortSetting -> IO Handle
openSerialPort = U.openSerialPort

-- | Opens the serial port and executions the action and makes sure to close the serial port
--   afterwards.
withSerialPort :: FilePath -> SerialPortSetting -> (Handle -> IO a) -> IO a
withSerialPort portFile settings = bracket (openSerialPort portFile settings) hClose
