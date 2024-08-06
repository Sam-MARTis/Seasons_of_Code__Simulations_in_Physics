# 2000 Body galaxy simulation

### How to run
You have two methods:
1) Simply run the file provided
2) Make changes to the parametes(ex: Different no of particles)

For both, first clone the repo
```bash
git clone git@github.com:Sam-MARTis/Fortran.git
```

Next navigate to the correct folder:

```bash
cd Projects/11_Galaxy-Sims/Basic_Galaxy/
```

Next, run the pre built executable and wait as your computer crunches the numbers.
(In case you get a different simulation, run the a.out file instead)

```bash
./galaxy_sim_2k
```

If you wish to change parameters you need tor ecompile the file. 
To do this, open the `core.f95` file in your favourite editor and scroll down to the main program.

There, you can change the number of particle, dt, G and mass of central heavy object (black hole, in context of galaxy sims)



```fortran
    integer, parameter:: noOfBodies = 2000 !Pretty self-explanatory name
    real, parameter:: dt = 0.01 !Time step
    real, parameter:: G = 0.05 !Gravitational constant
    real, parameter:: mainMass = 100000.0 !Mass of central object
    real, parameter:: theta_max = 1.5 !Affects accuracy and speed. Higher is faster but less accurate
    integer, parameter:: iterations = 50000 !Iterations count. Iterations * dt = simulation length
```


