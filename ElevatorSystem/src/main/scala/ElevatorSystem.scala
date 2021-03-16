
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, TimeoutException}
import scala.language.postfixOps
import scala.math.abs

object ElevatorSystem {
  def numberOfFloors = 10

  var isWorking = true //current state of system
  var isOn: Boolean = false //current state of simulation
  var numberOfElevators = 0
  val system: ActorSystem = ActorSystem("Elevators")
  implicit val timeout: Timeout = Timeout(30 seconds) //maximum waiting time for the response

  case class ButtonClicked(ID: Int, which: Int) //signal that simulates the elevator panel
  case class PickUp(floor: Int, direction: Int) //class defined to send data from users to ElevatorSystem
  case class AskForPosition() //class defined to collect data from Elevators
  case class AskForStatus() //class defined to ask for status of all Elevators
  case class Update() //class defined to update elevator status
  case class Step() //class defined to continue simulation
  case class Request(floor: Int) //class defined to give the elevator next destination
  case class End() //class defined to close the program
  case class StartAndStop() //class defined for simulation purposes

  def NumberChooser(min: Int, max: Int): Int = { //Simple input handling function, checking if value fits in (min,max] range

    try {
      val numberOfElevators = scala.io.StdIn.readLine().toInt
      if (numberOfElevators <= max && numberOfElevators > min) numberOfElevators
      else {
        println(s"Number must be in range of ($min,$max]. Try again.")
        NumberChooser(min, max)
      }
    } catch {
      case _: NumberFormatException =>
        println(s"Number must be in range of ($min,$max]. Try again.")
        NumberChooser(min, max)
    }
  }


  class ElevatorSystemAdmin(numberOfElevators: Int) extends Actor { //class defining default behaviour on system start and handling user data
    var elevatorList: List[ActorRef] = List[ActorRef]()

    for (i <- 0 until numberOfElevators) {
      elevatorList = elevatorList :+ system.actorOf(Props(classOf[Elevator], i + 1)) //creating a list of elevators
    }
    val elevatorGuardian: ActorRef = system.actorOf(Props(classOf[ElevatorGuardian], elevatorList), "Guardian")
    val timer: ActorRef = system.actorOf(Props(classOf[SimulationTimer], self), "Timer")

    def receive: Receive = {
      case AskForStatus() => //after receiving this signal Admin will ask Guardian for elevators status

        try {
          val future = elevatorGuardian ? AskForStatus()
          val result = Await.result(future, timeout.duration)
          sender() ! result
        } catch {
          case _: TimeoutException => //error
        }

      case Step() => //after getting this Admin would inform Guardian that another step of simulation must be made
        elevatorGuardian ! Step()

      case m: PickUp => //After receiving this Admin will tell the Guardian about new pickup
        elevatorGuardian ! m

      case End() => //beginning of closing procedure
        if (isOn) timer ! StartAndStop()
        elevatorGuardian ! End()

      case m: ButtonClicked => //after this signal admin will tell the guardian the ID of the elevator and number of floor that it has to go
        elevatorGuardian ! m

      case StartAndStop() => timer ! StartAndStop() //starting the simulation

      case _ => println("ADMIN ERROR  ")
    }
  }

  object ElevatorSystemAdmin {
    def apply(numberOfElevators: Int): ElevatorSystemAdmin = new ElevatorSystemAdmin(numberOfElevators)
  }

  class SimulationTimer(systemAdmin: ActorRef) extends Actor { //class for simulation purposes only, takes care of time flow
    def receive: Receive = {

      case StartAndStop() => //starting simulation, 10 seconds = one step
        if (!isOn) println("Simulation started")
        else println("Simulation stopped")
        isOn = !isOn
        self ! Step()

      case Step() => //signal parsed every 10 seconds to simulate everything
        if (isOn) {
          Thread.sleep(10_000)
          systemAdmin ! Step()
          val generator = scala.util.Random
          for (_ <- 0 until generator.nextInt(numberOfElevators + 1)) { //randomly call from 0 to numberOfElevators elevators
            systemAdmin ! PickUp(generator.nextInt(numberOfFloors + 1), generator.nextInt(5) - 9)
          }


          val future = (systemAdmin ? AskForStatus()).mapTo[List[(Int, Int, List[Int])]]
          val result = Await.result(future, timeout.duration)
          println(result.map(x => s"ID:${x._1} Floor:${x._2} Destinations:${x._3.mkString(",")}").mkString("\n" + "-" * 25 + "\n") + "\n") //text formatted to more friendly-looking style

          self ! Step()
        }

      case _ => println("TIMER ERROR")
    }

  }

  class ElevatorGuardian(elevatorList: List[ActorRef]) extends Actor { //guardian handling data from elevators
    def receive: Receive = {
      case AskForStatus() => //after receiving this signal Guardian will send the data about elevators to Admin
        var resultList = List[Any]()
        for (current <- elevatorList) {
          try {
            val future = current ? AskForPosition
            val result = Await.result(future, timeout.duration / 16)
            resultList = resultList :+ result
          } catch {
            case _: TimeoutException => resultList = resultList :+ (404, 404, List(404)) //if problem occurred while connecting to the elevator, all values would be 404
          }

        }
        sender() ! resultList

      case Step() => //after getting this signal the state of all elevators will be updated
        for (current <- elevatorList) {
          current ! Update
        }

      case m: PickUp => //after receiving this signal Guardian will choose the best elevator to pick up
        var bestElevatorCost = 1000
        var bestElevatorId = 1000
        for (x <- elevatorList.indices) { //finding the most appropriate elevator to handle the request
          try {
            val future = (elevatorList(x) ? m).mapTo[Int]
            val result = Await.result[Int](future, timeout.duration)
            if (result <= bestElevatorCost) {
              bestElevatorCost = result
              bestElevatorId = x
            }
          } catch {
            case _: RuntimeException => self ! m //trying again if it taking too long
          }
        }
        if (bestElevatorCost == 1000) {
          println("Elevators overcrowded, please wait for a moment ...")

        }
        else {
          Thread.sleep(100)
          elevatorList(bestElevatorId) ! Request(m.floor)
        }

      case End() => //after receiving this Guardian will immediately take every elevator on the ground
        self ! Step()
        for (x <- elevatorList) {
          x ! End()
        }
        for (_ <- 0 until numberOfFloors) {
          self ! Step()
        }
        context.system.terminate() //system shutdown

      case m: ButtonClicked => //after receiving this guardian tells the elevator number m.ID to go on m.which floor
        elevatorList(m.ID) ! Request(m.which)

      case _ => println("GUARDIAN ERROR")
    }

  }

  object ElevatorGuardian {
    def apply(elevatorList: List[ActorRef]): ElevatorGuardian = new ElevatorGuardian(elevatorList)
  }

  class Elevator(ID: Int) extends Actor { //class representing behaviour of single elevator

    def DoorOpener() { //function that simulates elevator doors
      println(s"Door in Elevator $ID opened")
      //in this place would be a door-opening system
    }

    def NumberClicked(floor: Int) { //function that simulates elevator panel
      if (!destination.contains(floor)) destination = destination :+ floor
    }

    def ChooseDirection(destination: List[Int] = destination, currentFloor: Int = currentFloor): Int = { //simple function to choose direction of move
      if (destination.isEmpty || destination.contains(currentFloor)) {
        0
      }
      else {
        (destination.head - currentFloor) / abs(currentFloor - destination.head)
      }
    }

    def CalculateCost(m: PickUp): Int = { //function checking the lowest cost of pickup, simulating numberOfFloors steps
      var currentScore = 1000
      var tempFloor = currentFloor
      var tempDestination = destination
      var tempDirection = currentDirection
      for (x <- 0 to numberOfFloors) {

        if (tempDestination.contains(tempFloor)) tempDestination = tempDestination.filter(_ != tempFloor)//simulating Update() signal
        tempDirection = ChooseDirection(tempDestination, tempFloor)
        tempFloor += tempDirection
        if (tempDirection == 0 || (tempFloor < m.floor && tempDirection == +1 && m.direction == +1) || (tempFloor > m.floor && tempDirection == -1 && m.direction == -1)) { // Function is ready for user input and respect if user wants to go upwards or downwards after entering elevator
          val tempScore = abs(tempFloor - m.floor) + x  //simply calculated cost of request: 1 point for each floor to between elevator and destination and for each simulation step.
          if (tempScore < currentScore) currentScore = tempScore
        }

      }
      currentScore
    }

    var currentFloor = 0
    var currentDirection = 0 //-1 means downwards, 1 means upwards, 0 means that elevator is staying still
    var destination: List[Int] = List[Int]()
    var doorOpened: Boolean = false //boolean simulating the current state of doors

    def receive: Receive = {

      case AskForPosition => //after getting this signal elevator will send data to the sender
        sender ! (ID, currentFloor, destination)

      case Update => //after this signal elevator continues working and check if is on requested floor
        if (destination.contains(currentFloor)) { //checking if current floor is on destination list
          destination = destination.filter(_ != currentFloor)
          DoorOpener()
        }
        currentDirection = ChooseDirection()
        currentFloor += currentDirection


      case m: PickUp => //after getting this elevator will calculate if is valid for this pickup request and return it cost
        sender() ! CalculateCost(m)

      case m: Request => //after this signal elevator will add new destination to the list
        if (!destination.contains(m.floor)) destination = destination :+ m.floor

      case End() => //after this the Elevator will go back to ground level
        destination = List[Int](0)

      case _ => println(s"ELEVATOR $ID ERROR")

    }
  }

  object Elevator {
    def apply(ID: Int): Elevator = new Elevator(ID)
  }

  @scala.annotation.tailrec
  def pickUpRequested(): PickUp = { //Function for handling user Input after pickUp
    print("Enter number of floor: ")
    try {
      var floor = scala.io.StdIn.readLine().toInt
      print("Enter negative number for downwards and positive for upwards: ")
      if (floor < 0) floor = 0
      else if (floor > numberOfFloors) floor = numberOfFloors //conditional to make simulation faster

      var destination = scala.io.StdIn.readLine().toInt
      if (destination < 0) destination = -1
      else if (destination >= 0) destination = +1
      PickUp(floor, destination)
    } catch {
      case _: NumberFormatException => pickUpRequested()
    }
  }


  def main(args: Array[String]) {
    print("Enter the number of  elevators[1,16]:")
    numberOfElevators = NumberChooser(0, 16)
    val elevatorSystem = system.actorOf(Props(classOf[ElevatorSystemAdmin], numberOfElevators), "Admin")
    println("1.Pickup\n2.Step\n3.Status\n4.Open Elevator Panel\n5.Show Context menu\n6.Switch Simulation\n7.End")
    while (isWorking) {

      scala.io.StdIn.readLine() match { //simple menu

        case "1" => elevatorSystem ! pickUpRequested()

        case "2" => elevatorSystem ! Step()

        case "3" => try {
          val future = (elevatorSystem ? AskForStatus()).mapTo[List[(Int, Int, List[Int])]]
          val result = Await.result(future, timeout.duration)
          println(result.map(x => s"ID:${x._1} Floor:${x._2} Destinations:${x._3.mkString(",")}").mkString("\n" + "-" * 25 + "\n")) //text formatted to more friendly-looking style
        } catch {
          case _: TimeoutException => println("System is currently too busy, try again later.")
        }

        case "4" =>
          print("Enter the elevator ID:")
          val ID = NumberChooser(0, numberOfElevators) - 1
          print(s"Enter number of floor:[0-$numberOfFloors]")
          val floor = NumberChooser(0, numberOfFloors)
          elevatorSystem ! ButtonClicked(ID, floor)


        case "5" =>
          println("1.Pickup\n2.Step\n3.Status\n4.Open Elevator Panel\n5.Show Context Menu\n6.Switch Simulation\n7.End")

        case "6" => elevatorSystem ! StartAndStop()

        case "7" =>
          isWorking = false
          elevatorSystem ! End()

        case _ => println("Wrong input")
      }
    }

  }
}

