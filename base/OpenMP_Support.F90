module OpenMP_Support
    implicit NONE
    integer :: yDim, numGrids, i, step, count, numOfFirstSize, numOfSecondSize, firstSize, secondSize
    integer, allocatable :: lower_upper_index(:,:)

    CONTAINS
    
    subroutine FindBounds(yDim, numGrids, lower_upper_index)
        implicit NONE
        integer :: yDim, numGrids, i, step, count, numOfFirstSize, numOfSecondSize, firstSize, secondSize
        integer, allocatable :: lower_upper_index(:,:)
        allocate(lower_upper_index(numGrids,2)) 
        
        ! if the size of each grid is the same
        if (modulo(yDim, numGrids) == 0) then
            step = yDim/numGrids
            count = 1
            ! go from 1-yDim incrementing by step size
            do i = 1, yDim, step
            lower_upper_index(count,1) = i
            lower_upper_index(count,2) = i + step - 1
            count = count + 1
            end do
        ! if at least one grid is a different size
        else 
            firstSize = yDim/numGrids 
            numOfSecondSize = modulo(yDim, numGrids)
            numOfFirstSize = numGrids - numOfSecondSize
            secondSize = (yDim - firstSize * numOfFirstSize) / numOfSecondSize
            
            count = 1
            do i = 1, numOfFirstSize * firstSize, firstSize 
            lower_upper_index(count,1) = i
            lower_upper_index(count,2) = i + firstSize - 1
            count = count + 1
            end do

            do i = numOfFirstSize * firstSize + 1, yDim, secondSize
            lower_upper_index(count,1) = i   
            lower_upper_index(count,2) = i + secondSize - 1
            count = count + 1
            end do
        end if
    end subroutine
end module
