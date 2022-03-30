#include <SPI.h>
#include <Wire.h>
#include <RH_RF95.h>

#define RFM95_CS 5
#define RFM95_RST 2
#define RFM95_INT 4
#define RF95_FREQ 915.0

// Singleton instance of the radio driver
RH_RF95 rf95(RFM95_CS, RFM95_INT);

// Need this on Arduino Zero with SerialUSB port (eg RocketScream Mini Ultra Pro)
//#define Serial SerialUSB

int led = 9;

void setup() 
{
  // Rocket Scream Mini Ultra Pro with the RFM95W only:
  // Ensure serial flash is not interfering with radio communication on SPI bus
  //  pinMode(4, OUTPUT);
  //  digitalWrite(4, HIGH);

  // manual reset
  digitalWrite(RFM95_RST, LOW);
  delay(10);
  digitalWrite(RFM95_RST, HIGH);
  delay(10);

  Serial.begin(9600);

  // LoRa Radio Init
  Serial.println("Arduino LoRa TX Test!");
  
  while (!Serial); // Wait for serial port to be available
  if (!rf95.init())
    Serial.println("init failed");  
  
  Serial.println("LoRa radio init OK!");

  // Defaults after init are 434.0MHz, modulation GFSK_Rb250Fd250, +13dbM
  if (!rf95.setFrequency(RF95_FREQ)) {
    Serial.println("setFrequency failed");
    while (1);
  }

  Serial.print("Set Freq to: "); Serial.println(RF95_FREQ);

  // The default transmitter power is 13dBm, using PA_BOOST.
  // If you are using RFM95/96/97/98 modules which uses the PA_BOOST transmitter pin, then 
  // you can set transmitter powers from 5 to 23 dBm:
  rf95.setTxPower(23, false);
  Wire.begin();
}

void loop()
{
  if (rand() % 50 == 1){
    Serial.println("Waiting for message");
  }
  
  if (rf95.available())
  {
    // Should be a message for us now   
    uint8_t buf[RH_RF95_MAX_MESSAGE_LEN];
    uint8_t len = sizeof(buf);
    if (rf95.recv(buf, &len))
    {
      digitalWrite(led, HIGH);
      RH_RF95::printBuffer("request: ", buf, len);
      Serial.print("got request: ");
      Serial.println((char*)buf);
      Serial.print("RSSI: ");
      Serial.println(rf95.lastRssi(), DEC);
      
      // Send a reply
      uint8_t data[] = "And hello back to you";
      rf95.send(data, sizeof(data));
      rf95.waitPacketSent();
      Serial.println("Sent a reply");
      digitalWrite(led, LOW);
    }
    else
    {
      Serial.println("recv failed");
    }
  }
}
