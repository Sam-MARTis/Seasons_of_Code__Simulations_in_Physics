The Goal of this Project is to create a N-body gravity simulator (N>500) with an attempt at implementing advanced algorithms.

The Goal is divided into four major milestones

 1) Implement k-d trees datastructure in Fortran

 2) Implement Barnes-Hut Algorithm 

 3) Implement Fast-Multipole-Method in Fortran

Each step comes with its own challenges. And thus, in the possible chance of being unable to implement the above, a simpler model is first constructed. Starting with N = 2 and scaling up to as much as a naive gravity $O(n^2)$ algorithm will allow.


The core fortran code will be responsible for the bulk of the computation, including finding the updates body states given the current states. The script will then write this data onto a 'stateData.txt' file along with the timestamp of the current state.


A C++ script will then later read the data of the stateData.txt file and visualize the data using a media library(At the moment, I am planning on using SFML cause of its ease of use) 

The timeline for the completetion of the project is as follows(Note, this is subject to minor change):

- Complete 2-body and naive gravity simulator by July $7^{th}$

- Implement k-d trees(Quad trees, specifically) by July $12^{th}$

- Implement Barnes-Hut algorithm by July $17^{th}$

- FMM by July $20^{th}$


Note, all goals except FMM have been completed. FMM was discarded due to the bottleneck being the write speed of the laptop to the file rather than the computations themselves.


