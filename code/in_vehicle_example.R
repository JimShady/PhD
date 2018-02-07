_## Code by James Smith, 22 September 2015
## Example below is for when a subject is on the DLR (vehicle mode 18)
## The output is the concentration at 7:57, two minutes after
## the journey started at 7:55
## The process is similar for other journey types
## Using NOX pollutant properties as proxy for journey_start_no2
## Replicating journey (ssid) 505004171020106

## pollutant properties
nox_d_vel           <- 0.432 ## deposition velocity
nox_res_r_zr        <- 0 ## nox resuspension rate

## vehicle properties
dep_sur             <- 74 ## internal surface area of the vehicle
no_ps               <- 70 ## number of passengers in vehicle
v_vol               <- 260 ## volume of the vehicle
exc_nat_zr          <- 5.2 ## natural hourly air exchange rate at start of the journey
fil_ef              <- 1 ## vehicle filter effeciency
exc_mec             <- 0 ## mechanical hourly air exchange rate
exc_nat             <- 10 ## natural hourly air exchange rate
no_acps             <- 30 ## number of active passengers in vehicle
passenger_surface_area    <- 2.92 ## surface area of a passenger

## Conditions
journey_start_no2   <- 120.2343160622219292  # Outdoor concentration at the start of the journey
journey_start_nox   <- 296.4997652826462043  # Outdoor concentration at the start of the journey
epoch_current_time  <- 1252052460            # Time at which the journey in question started (#7:57)
epoch_journey_start_time  <- 1252052400            # Time at which we want to know indoor concentration (#7:55)
current_no2         <- 49.1132098585253775   # Outdoor concentration taken from CMAQ-Urban
current_nox         <- 92.4817114798921030   # Outdoor concentration taken from CMAQ-Urban

lambda.win  <- exc_nat
n           <- fil_ef
lambda.hvac <- exc_mec
V.g         <- nox_d_vel
A.star      <- dep_sur + passenger_surface_area*no_ps
V           <- v_vol
no2.C.out   <- current_no2
nox.C.out   <- current_nox
Q           <- no_acps * nox_res_r_zr
lambda.win.star     <- 0.5
lambda.win0 <- exc_nat_zr
no2.C.out0  <- journey_start_no2
nox.C.out0  <- journey_start_nox
t           <- (epoch_current_time - epoch_journey_start_time+60)/3600

a       <-  lambda.win + n*lambda.hvac + V.g*(A.star/V)
a0      <-  lambda.win0 + n*lambda.hvac + V.g*(A.star/V)

no2_b   <-  lambda.win*no2.C.out + Q/V
no2_b0  <-  lambda.win0*no2.C.out0 + Q/V

nox_b   <-  lambda.win*nox.C.out + Q/V
nox_b0  <-  lambda.win0*nox.C.out0 + Q/V

no2_C.in0  <-  (lambda.win.star/(lambda.win.star + V.g*(A.star/V)))*no2.C.out0
nox_C.in0  <-  (lambda.win.star/(lambda.win.star + V.g*(A.star/V)))*nox.C.out0

no2_C.in    <-  (no2_C.in0 - no2_b0/a0)*exp(-a*t) + no2_b/a
nox_C.in    <-  (nox_C.in0 - nox_b0/a0)*exp(-a*t) + nox_b/a

#print(paste("a is", a))
#print(paste("a0 is", a0))
#print(paste("b is", b))
#print(paste("b0 is", b0))
#print(paste("C.in0 is", C.in0))
#print(paste("C.in is", C.in))
#print(t)
print(paste("no2 is ", round(no2_C.in,2)))
print(paste("nox is ", round(nox_C.in,2)))