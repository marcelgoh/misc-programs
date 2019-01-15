# Antipodal Temperatures

A small program that illustrates the [Borsuk-Ulam theorem](https://en.wikipedia.org/wiki/Borsuk–Ulam_theorem) by finding two antipodal points on the globe with the same temperature.  

## Usage
```
****************************************
Choose an option by entering a number:
1) Update to current weather data
2) Find antipodes in current weather data
3) Import data from file
4) Exit the program
1
Trying to find data from web...
Data fetched successfully from server.
****************************************
Choose an option by entering a number:
1) Update to current weather data
2) Find antipodes in current weather data
3) Import data from file
4) Exit the program
2
Analysing data on file...
At 01:40:00 GMT on 2019-01-15,
24.77 N 125.27 E and 25.0 S 53.5 W are antipodal with 0.6329% error
and both have temperature 22.0 degrees Celsius.
****************************************
Choose an option by entering a number:
1) Update to current weather data
2) Find antipodes in current weather data
3) Import data from file
4) Exit the program
3
Enter the relative path of a file:
Historical/2019-01-05.csv
Analysing data on file...
At 21:50:00 GMT on 2019-01-05,
13.42 N 103.83 E and 13.73 S 76.22 W are antipodal with 0.1743% error
and both have temperature 25.0 degrees Celsius.
****************************************
Choose an option by entering a number:
1) Update to current weather data
2) Find antipodes in current weather data
3) Import data from file
4) Exit the program
4
Closing program...
```

## Authors
Data: [Aviation Weather Centre](https://aviationweather.gov) (National Oceanic and Atmospheric Administration)  
Code: Marcel Goh
