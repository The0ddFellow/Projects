#include <SPI.h>
#include <LoRa.h>
#include <Wire.h>
#include <Adafruit_MPL115A2.h>
#define USE_ARDUINO_INTERRUPTS true
Adafruit_MPL115A2 mpl115a2;
const int csPin = 5;
const int resetPin = 2;
const int irqPin = 4;
byte localAddress = 0xBB;
byte destinationAddress = 0xAA;
long lastSendTime = 0;
int interval = 2000;
int count = 0;

void setup() {
  Serial.begin(9600);
  mpl115a2.begin();
  Serial.println("Start LoRa duplex");
  Serial.print("Local address: ");
  Serial.println(String(localAddress, HEX));
  Serial.print("Destination address: ");
  Serial.print(String(destinationAddress, HEX));
  LoRa.setPins(csPin, resetPin, irqPin);
  if (!LoRa.begin(915E6)) {
    Serial.println("LoRa init failed. Check your connections.");
    while (true) {}
  }
}
void loop() {
  // Temperature Sensor
  float pressureKPA = 0, temperatureC = 0;
  mpl115a2.getPT(&pressureKPA, &temperatureC);

  if (millis() - lastSendTime > interval) {
    String temperatureData = String(temperatureC);
    String pressureData = String(pressureKPA);
    sendMessage(temperatureData);
    sendMessage(pressureData);
    Serial.print("Sending data " + temperatureData + " and " + pressureData);
    Serial.print(" from source 0x" + String(localAddress, HEX));
    Serial.println(" to destination 0x" + String(destinationAddress, HEX));
    lastSendTime = millis();
    interval = random(2000) + 1000;
  }
  receiveMessage(LoRa.parsePacket());
}
void sendMessage(String Data) {
  LoRa.beginPacket();
  LoRa.write(destinationAddress);
  LoRa.write(localAddress);
  LoRa.write(Data.length());
  LoRa.print(Data);
  LoRa.endPacket();
}
void receiveMessage(int packetSize) {
  if (packetSize == 0) return;
  int recipient = LoRa.read();
  byte sender = LoRa.read();
  byte incomingLength = LoRa.read();
  String incoming = "";
  while (LoRa.available()) {
    incoming += (char)LoRa.read();
  }
  if (incomingLength != incoming.length()) {
    Serial.println("Error: Message length does not match length");
    return;
  }
  if (recipient != localAddress) {
    Serial.println("Error: Recipient address does not match local address");
    return;
  }
  Serial.print("Received data " + incoming);
  Serial.print(" from 0x" + String(sender, HEX));
  Serial.println(" to 0x" + String(recipient, HEX));
}
