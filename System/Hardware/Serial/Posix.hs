module System.Hardware.Serial.Posix (
    openSerialPort
) where

import System.Hardware.Serial.Settings
import System.Posix.IO
import System.Posix.Terminal hiding (BaudRate(..))
import qualified System.Posix.Terminal as T
import System.IO
import qualified Data.ByteString as BS
import Control.Exception


convBaudRate :: BaudRate -> T.BaudRate
convBaudRate speed = case speed of
        B110    -> T.B110
        B300    -> T.B300
        B600    -> T.B600
        B1200   -> T.B1200
        B2400   -> T.B2400
        B4800   -> T.B4800
        B9600   -> T.B9600
        B19200  -> T.B19200
        B38400  -> T.B38400
        B57600  -> T.B57600
        B115200 -> T.B115200


withStopBits :: TerminalAttributes -> StopBits -> TerminalAttributes
withStopBits termOpts One = termOpts `withoutMode` T.TwoStopBits
withStopBits termOpts Two = termOpts `withMode` T.TwoStopBits

withParity :: TerminalAttributes -> Parity -> TerminalAttributes
withParity termOpts Even = termOpts `withMode` EnableParity `withoutMode` OddParity
withParity termOpts Odd = termOpts `withMode` EnableParity `withMode` OddParity
withParity termOpts NoParity = termOpts `withoutMode` EnableParity

configure :: TerminalAttributes -> BaudRate -> Int -> StopBits ->
                Parity -> FlowControl -> TerminalAttributes
configure termOpts baud bits stop parity flow =
    termOpts `withInputSpeed` convBaudRate baud
             `withOutputSpeed` convBaudRate baud
             `withBits` bits
             `withStopBits` stop
             `withParity` parity
             `withoutMode` T.StartStopInput
             `withoutMode` T.StartStopOutput
             `withoutMode` T.EnableEcho
             `withoutMode` T.EchoErase
             `withoutMode` T.EchoKill
             `withoutMode` T.ProcessInput
             `withoutMode` T.ProcessOutput
             `withoutMode` T.MapCRtoLF
             `withoutMode` T.EchoLF
             `withoutMode` T.HangupOnClose
             `withoutMode` T.KeyboardInterrupts
             `withoutMode` T.ExtendedFunctions
             `withMode` T.LocalMode
             `withMode` T.ReadEnable
             `withTime` 1
             `withMinInput` 0


configureFd :: FilePath -> SerialPortSetting -> IO ()
configureFd portFile s = do
        fd <- openFd portFile ReadWrite Nothing flags
        ta <- getTerminalAttributes fd
        let ta' = configure ta (baudRate s) (bitsPerWord s) (stopBits s) (parity s) (flowControl s)
        setTerminalAttributes fd ta' Immediately
        closeFd fd
    where flags = OpenFileFlags True True True True False


openSerialPort :: FilePath -> SerialPortSetting -> IO Handle
openSerialPort portFile settings = do
        configureFd portFile settings
        h <- openBinaryFile portFile ReadWriteMode
        hSetBuffering h NoBuffering
        return h
