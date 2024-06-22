## Pretty big assignment

So we finally got serious with the assignments, and got to the core of the SoC. 
I had fun. 

Most of my code is modular so I didn't have a lot of unecessary headache in debugging, although I did spend a solid five six hours figuring out why tf shit wasnt working.

I implemented the jacobi and gauss-seidel solver. I then reimplemented cause the first time, u had to hard code the equation into it. In the second time, you could just supply the matrix and it'll do the rest.

Used the latter version for the poisson solvers.

Once the conventional metrix method was applied, I also took upon the extra task of making a sparse matrix implementation.

That took a while becuase I had took a break to complete some other work and by the time I came back, I looked at my code like... "When did I write all of this"

After I reminded myself of what I had done, I tried a couple of methods of making an adjacent matrix. The main issue I faced was that arrays in fortran must be uniform. You can't have something like [[1,2,3,4], [8,4], [1], [24, 1,2]]

So it took a bit of manuevering to get that right. I ended up storing a 4 x $n^2$ x 2 matrix. The 3'd slice's 1st entry describes which state variable (ex: $U_{4,5}$) to add and 2nd entry describes its multiplier.
The 1st slice's 4 entries are for the four state components the current state depends on.
The more general approach would be to not have fixed dimensions, but I couldn't figure out a way of having a combination of results, efficiency and simplicity.

Especially cause of the above mentioned issue of not having non-uniform arrays in fortran.

There was an additional task of implementing the above in MATLAB, but due to time constraints(Other projects + SoS submission) and the fact that MATLAB had left me scarred the last time I used it (Mathworks mini drone conntest, as a team member of UMIC. As soon as it was over, i 'rm -rf'd and purged everything matlab related)

One of the modules required for the heatmap doesn't work for me, so i instead used a graph. I scoured stackoverflow and google for a solution, but didnt find anything. 

I ran the code in a VM and got the following images for different charge distributions


![Dist 1](assets/dist1.png)

![Dist 2](assets/dist2.png)

![Dist 3](assets/dist3.png)

The last one is $\sigma$(x,y)= (1/3) * (sin(x/50)*sin(y/50))


For running any of the code, simply run the a.out file


