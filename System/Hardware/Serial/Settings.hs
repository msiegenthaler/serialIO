module System.Hardware.Serial.Settings (
    SerialPortSetting(..),
    defaultSettings,
    BaudRate(..),
    StopBits(..),
    Parity(..),
    FlowControl(..)
) where


-- | Communication speed used on the serial port.
data BaudRate = B110
              | B300
              | B600
              | B1200
              | B2400
              | B4800
              | B9600
              | B19200
              | B38400
              | B57600
              | B115200 deriving (Show,Eq)

-- | Stop bits sent at the end of every character allow the receiving signal hardware to detect
--   the end of a character and to resynchronise with the character stream.
data StopBits = One | Two deriving (Show,Eq)

-- | When parity is used with a serial port, an extra data bit is sent with each data character,
--   arranged so that the number of 1 bits in each character, including the parity bit, is
--   always odd or always even
data Parity = Even | Odd | NoParity deriving (Show,Eq)

-- | Hardware flow control is not supported at the moment.
data FlowControl = Software | NoFlowControl deriving (Show,Eq)

-- | Settings for the serial port.
data SerialPortSetting = SerialPortSetting {baudRate :: BaudRate,
                                            bitsPerWord :: Int,
                                            stopBits :: StopBits,
                                            parity :: Parity,
                                            flowControl :: FlowControl }

-- | Default Settings (9600, 8 bits, 1 stop bit, no parity, no flow control)
defaultSettings = SerialPortSetting B9600 8 One NoParity NoFlowControl