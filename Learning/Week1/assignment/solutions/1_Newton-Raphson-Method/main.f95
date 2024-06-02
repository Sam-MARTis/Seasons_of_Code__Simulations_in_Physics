
program main
    use customFunctions, only: find_E, f1, wp
    use Newton_Raphson, only: findZero

    
    real(kind=wp):: sol
    ! real(kind=wp):: f1
    real(kind=wp):: initalGuess
    real(kind=wp):: E
    initalGuess = 1.4
    print *, f1(initalGuess)

    sol = findZero(f1, initalGuess)
    print *, sol
    E = find_E(sol)
    print *, E

    
    

end program main