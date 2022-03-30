/*
  Name Print

 This sketch prints the temperature and humidity of the room on an LCD screen.
 
 by Javier Rodriguez
 
*/

// Library Codes
#include <LiquidCrystal.h>
#include <dht.h>

// define where the analog pin for sensor is
#define dht_apin A0

// Initialize class
dht DHT;  

// initialize the library with the numbers of the interface pins
LiquidCrystal lcd(7, 8, 9, 10, 11, 12);


void setup() {
  // set up the LCD's number of columns and rows
    lcd.begin(16, 2);

  // Print a message to the LCD
  lcd.print("Temp: ");

  // Set cursor to second line
  lcd.setCursor(0,1);
  lcd.print("Humidity: ");

}

void loop() {
  // Read current values
  DHT.read11(dht_apin);
  
  // Set cursor for temp
  lcd.setCursor(6,0);
  lcd.print(DHT.temperature);
  lcd.print("C");

  //Set cursor for humidity
  lcd.setCursor(10, 1);
  lcd.print(DHT.humidity);
  lcd.print("%");
}
