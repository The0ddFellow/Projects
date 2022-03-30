/*
  TinyCircuits Si7020 Temperature and Humidity Sensor TinyShield Example Sketch

  This demo shows the bare minimum to read temperature and humidity data from
  the Si7020 sensor using the Si7020 library written by Marcus Sorensen.

  This example is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  Written 20 March 2016
  By Ben Rose
  Modified 07 January 2019
  By Hunter Hykes

  https://TinyCircuits.com
*/
#include <Wire.h>
#include <SPI.h>
#include <stdio.h>
#include <RH_RF95.h>
#include <stdlib.h>

//DEFINITIONS ===============================

//#if defined (ARDUINO_ARCH_AVR)
//#define SerialMonitorInterface Serial
//#include <SoftwareSerial.h>
//#elif defined(ARDUINO_ARCH_SAMD)
//#define SerialMonitorInterface Serial
//#include "SoftwareSerialZero.h"
//#endif


#define TSL2572_I2CADDR     0x39
#define GAIN_1X 0
#define GAIN_8X 1
#define GAIN_16X 2
#define GAIN_120X 3

//only use this with 1x and 8x gain settings
#define GAIN_DIVIDE_6 true

//===END DEFINITIONS========================

//VARIABLE DECLARATIONS=====================

// Connect LoRaRFM_9X_Adafruit
#define RFM95_CS 5
#define RFM95_RST 2
#define RFM95_INT 4
// End Connect LoRaRFM_9X_Adafruit

// Change to 434.0 or other frequency, must match RX's freq!
#define RF95_FREQ 915.0

// Singleton instance of the radio driver
RH_RF95 rf95(RFM95_CS, RFM95_INT);

// Set Collar and Bear Identification Numbers
int collar_id = 1;
int bear_id = 1;

int gain_val = 0;
float celcius;
float relativeHumidity;
float f_temp;
float AmbientLightLux;
float pressure;
char str_temp[7];
char str_humd[7];
char str_light[9];
char str_press[8];
char str_flat[9];
char str_flon[10];
uint8_t data_buf[250];
int Satellites = 0;

// ===END VARIABLE DECLARATIONS==============

void setup()
{

  pinMode(RFM95_RST, OUTPUT);
  digitalWrite(RFM95_RST, HIGH);

  Serial.begin(115200);
  delay(100);

  // LoRa Radio Init
  Serial.println("Arduino LoRa TX Test!");

  // manual reset
  digitalWrite(RFM95_RST, LOW);
  delay(10);
  digitalWrite(RFM95_RST, HIGH);
  delay(10);

  while (!rf95.init()) {
    Serial.println("LoRa radio init failed");
    while (1);
  }
  Serial.println("LoRa radio init OK!");

  // Defaults after init are 434.0MHz, modulation GFSK_Rb250Fd250, +13dbM
  if (!rf95.setFrequency(RF95_FREQ)) {
    Serial.println("setFrequency failed");
    while (1);
  }
  Serial.print("Set Freq to: "); Serial.println(RF95_FREQ);

  // Defaults after init are 434.0MHz, 13dBm, Bw = 125 kHz, Cr = 4/5, Sf = 128chips/symbol, CRC on

  // The default transmitter power is 13dBm, using PA_BOOST.
  // If you are using RFM95/96/97/98 modules which uses the PA_BOOST transmitter pin, then
  // you can set transmitter powers from 5 to 23 dBm:
  rf95.setTxPower(23, false);
  // End LoRa Radio Init Sequence

  Wire.begin();
}

void loop()
{
  Serial.println("Sending...");
  uint8_t data[] = "And hello back to Javier";
  rf95.send(data, sizeof(data));

  Serial.println("Waiting for packet to complete...");
  delay(10);
  rf95.waitPacketSent();
  // Now wait for a reply
  uint8_t buf[RH_RF95_MAX_MESSAGE_LEN];
  uint8_t len = sizeof(buf);

  Serial.println("Waiting for reply...");
  delay(10);
  if (rf95.waitAvailableTimeout(1000))
  {
    // Should be a reply message for us now
    if (rf95.recv(buf, &len))
    {
      Serial.print("Got reply: ");
      Serial.println((char*)buf);
      Serial.print("RSSI: ");
      Serial.println(rf95.lastRssi(), DEC);
    }
    else
    {
      Serial.println("Receive failed");
    }
  }
  else
  {
    Serial.println("No reply, is there a listener around?");
  }
  delay(3000);
}
