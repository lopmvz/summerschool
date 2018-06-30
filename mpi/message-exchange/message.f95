program message
    use mpi
    implicit none
    integer, parameter :: n=5
    integer :: rc, rank, size, comm, i, status(mpi_status_size), tag, dest, source
    integer, allocatable :: array(:)

    call mpi_init(rc)
    call mpi_comm_rank(comm, rank, rc)
    call mpi_comm_size(comm, size, rc)

    allocate(array(n))

    do i=1,n
        array(i) = rank
    enddo

    write(*,*) "I'm ", rank, ":", array(:)

    if (rank==0) then
        print *, "I'm sending sth..."
        dest = 1
        tag = 10
        call mpi_send(array, n, mpi_integer, 1, tag, comm, rc)
        write(*,*) array
    else if (rank == 1) then
        print *, "I'm receiving sth... "
        source = 0
        tag = 10
        !print *, "count is=",nc
        call mpi_recv(array, n, mpi_integer, 0, tag, comm, status, rc)
        write(*,*) array
    endif

    write(*,*) "I'm ", rank, ":", array(1)

    call mpi_finalize(rc)
    
end program message