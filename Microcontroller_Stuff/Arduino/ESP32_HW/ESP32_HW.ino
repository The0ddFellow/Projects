/* HOMEWORK IoT Fall 2020 */
// Libraries
#ifdef ESP32
  #include <WiFi.h>
  #include <HTTPClient.h>
#else
  #include <ESP8266WiFi.h>
  #include <ESP8266HTTPClient.h>
  #include <WiFiClient.h>
#endif
#include <Wire.h>
// ==> Replace with your wifi network credentials
const char* ssid     = "TP-LINK_4072";
const char* password = "Mushroombuddy5";
// Do not change; Keep as http://bryan.iotserver.website/index.php
const char* serverName = "http://bryan.iotserver.website/index.php";
// API Key used to validate incoming data 
String apiKeyValue = "tPmAT5Ab3j7F9";
// ==> Enter you name in lower case (i.e. bryan)
String first_name = "javier";
void setup() {
  Serial.begin(115200);
  
  WiFi.begin(ssid, password);
  Serial.println("Connecting");
  while(WiFi.status() != WL_CONNECTED) { 
    delay(500);
    Serial.print(".");
  }
  Serial.println("");
  Serial.print("Connected to WiFi network with IP Address: ");
  Serial.println(WiFi.localIP());
}
void loop() {
  
    //Check WiFi connection status
    if(WiFi.status()== WL_CONNECTED){
      // Homework Objective -- Create a script within this code block
      // that will alternate between sending two http requests: one 
      // that will turn the light on and one that will turn the light off.
      // Place a 1 second delay at the end of each http request.
      // The HTTP Post Request variables should include api_key=tPmAT5Ab3j7F9 , name=yourname_lowercase, and light_status=0 or 1.
      // Navigate to http://bryan.iotserver.website/index.php to see if it worked :)
      
      //  ==> Homework Code Goes Here...
      // 1. Send http request to turn light on
      if(WiFi.status()== WL_CONNECTED){
        HTTPClient http;
      
        // Your Domain name with URL path or IP address with path
        http.begin(serverName);
      
        // Specify content-type header
        http.addHeader("Content-Type", "application/x-www-form-urlencoded");
      
        // Prepare your HTTP POST request data
        String httpRequestData = "api_key=" + apiKeyValue + "&light_status=1" + "&name=" + first_name;
        Serial.print("httpRequestData: ");
        Serial.println(httpRequestData);
      
        // Send HTTP POST request
        int httpResponseCode = http.POST(httpRequestData);
          
        if (httpResponseCode>0) {
          Serial.print("HTTP Response code: ");
          Serial.println(httpResponseCode);
        }
        else {
          Serial.print("Error code: ");
          Serial.println(httpResponseCode);
        }
        // Free resources
        http.end();
    }
      
      // 2. delay(1000)
      delay(1000);
        
      // 3. Send htttp request to turn light off
        HTTPClient http2;
      
        // Your Domain name with URL path or IP address with path
        http2.begin(serverName);
      
        // Specify content-type header
        http2.addHeader("Content-Type", "application/x-www-form-urlencoded");
      
        // Prepare your HTTP POST request data
        String http2RequestData = "api_key=" + apiKeyValue + "&light_status=0" + "&name=" + first_name;
        Serial.print("http2RequestData: ");
        Serial.println(http2RequestData);
      
        // Send HTTP POST request
        int http2ResponseCode = http2.POST(http2RequestData);
          
        if (http2ResponseCode>0) {
          Serial.print("HTTP2 Response code: ");
          Serial.println(http2ResponseCode);
        }
        else {
          Serial.print("Error code: ");
          Serial.println(http2ResponseCode);
        }
        // Free resources
        http2.end();
      
      // 4. delay(1000)
      delay(1000);      
      
    } else {
      Serial.println("WiFi Disconnected");
    }
   delay(5);
 }
