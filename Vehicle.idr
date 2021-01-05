module Main

data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Unicycle : Vehicle Pedal
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Tram : (fuel : Nat) -> Vehicle Electric
  ElectricCar : (fuel : Nat) -> Vehicle Electric

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels Unicycle = 1
wheels (Car fuel) = 4
wheels (Bus fuel) = 6
wheels (Motorcycle fuel) = 2
wheels (Tram fuel) = 6
wheels (ElectricCar fuel) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 80


