/*
 Send And Recieve

 This sketch sends proximity data and recieves temperature data
 
 by Javier Rodriguez
 
*/

// Library Codes
#include <LiquidCrystal.h>

// initialize the library with the numbers of the interface pins
LiquidCrystal lcd(1, 2, 4, 5, 6, 7);

#define echoPin 13
#define trigPin 12

long duration; // variable for the duration of sound wave travel
int distance; // variable for the distance measurement
  
void setup() {
  // set up the LCD's number of columns and rows
  lcd.begin(16, 2);
  
  // sets the pins
  pinMode(trigPin, OUTPUT);
  pinMode(echoPin, INPUT);
}

void loop() {
  
  lcd.print("Distance: ");
  
  // Clears the trigPin condition
  digitalWrite(trigPin, LOW);
  delayMicroseconds(2);
  
  // Sets the trigPin HIGH (ACTIVE) for 10 microseconds
  digitalWrite(trigPin, HIGH);
  delayMicroseconds(10);
  digitalWrite(trigPin, LOW);
  
  // Reads the echoPin, returns the sound wave travel time in microseconds 
  duration = pulseIn(echoPin, HIGH);
  
  // Calculating the distance
  distance = (duration / 2) * 0.0343; // Speed of sound wave divided by 2 (go and back)

  lcd.setCursor(0, 1);
  lcd.print(min(distance, 999)); //making sure to stop printing at 2 digits
  lcd.print(" cm");

  delay(500);
  lcd.clear();
}
