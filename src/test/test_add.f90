program test_add
  use hdf5_interface
  implicit none

  type(hdf5_file) :: h5f
  integer, allocatable :: iv(:), iv_g(:)
  real, allocatable    :: rv(:), rv_g(:)


  iv = [1,2,3,4]
  rv = [1.,2.,3.,4.]

  call h5f%open('x.h5', status='old', action='write')

  call h5f%add('/vector/iv', iv)
  call h5f%add('/vector/rv', rv)

  call h5f%get('/vector/iv', iv_g)
  call h5f%get('/vector/iv', rv_g)

  call h5f%close(finalize=.TRUE.)

  if(all(iv_g == iv) .and. all(rv_g == rv)) print*, 'pass'

end program test_add
