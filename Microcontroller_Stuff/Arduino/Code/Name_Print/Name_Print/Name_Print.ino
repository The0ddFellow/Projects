/*
  Name Print

 This sketch prints the name of the user to the LCD.
 
 by Javier Rodriguez
 
*/

// Arduino joystick pin numbers
const int pr = 2; // digital pin connected to switch output
const int X_pin = A0; // analog pin connected to X output
const int Y_pin = A1; // analog pin connected to Y output

// Library Codes
#include <Wire.h>
#include <LiquidCrystal_I2C.h>
#include <AlignedJoy.h>

// initialize the library with the numbers of the interface pins
LiquidCrystal_I2C lcd(7, 8, 9, 10, 11, 12);

// new joystick object
AlignedJoy joystick_1(X_pin, Y_pin);

void setup() {
  pinMode(pr, INPUT);
  digitalWrite(pr, HIGH);

  // set up the LCD's number of columns and rows
  lcd.begin(16, 2);
  
  // Print a message to the LCD
  lcd.print("Name:");
}

// column number
int i = 0;

// Letter number
char j = 'A';

void loop() {
  
  // set the cursor to column i, line 1
    lcd.setCursor(i, 1);
  
  // when button is pressed, move column
  if (digitalRead(pr) == 0) {
    i++;
    delay(500);
  }

  // print the number of times up
  if (joystick_1.read(Y) < 11) {
    j++;
    delay(500);
  } else if (joystick_1.read(Y) > 1014) {
    j--;
    delay(500);
  }
  
  lcd.print(j);
}
