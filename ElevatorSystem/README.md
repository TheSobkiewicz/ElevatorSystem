## Table of contents
* [General info](#general-info)
* [Technologies](#technologies)
* [Setup](#setup)
* [Admin Panel](#Admin_Panel)

## General info
This project allows user to take control of elevator system simulation.
As an administrator, you have ability to request pick up on specific floor, see the status of every elevator, or even control chosen one. 
After every pickup request, program automatically pick up the best elevator and send it at requested floor. It also contains real-time simulation.
	
## Technologies
Project is created with:
* Scala Version: 2.13.5
* Akka Version:  2.6.10
* Sbt Version: 1.4.7
	
## Setup
To run this project, open it using sbt:

```
$ cd ../ElevatorSystem
$ sbt.bat
$ run
```
Or use Your favourite IDE.

## Admin Panel
As an admin of elevator system you have bunch of possible actions:
* 1. Pickup - pick up elevator on specific floor and determine which direction you want to go, system will pick the most-fitting elevator and add this floor to it directions. Program is using 10 step simulation to choose the best one.
* 2. Step - this option is responsible for starting next simulation step.
* 3. Status - after picking this option, the status of all elevators is shown.
* 4. Open Elevator Panel - after using this, user takes control of one of the elevators and send it to selected floor.
* 5. Show Context Menu - this option prints the system menu.
* 6. Switch simulation- on/off the real-time simulation of whole system, with random pickup requests.
* 7. End - this option immidiately takes all of the elevators on the ground level and shuts down the whole system. 