program test_string
  use hdf5_interface
  implicit none

  type(hdf5_file) :: h5f
  character(:), allocatable :: s, s_g


  s = '12345 67890'

  call h5f%open('x.h5', status = 'new')

  call h5f%add('/string', s)
  call h5f%get('/string', s_g)

  call h5f%close(finalize=.TRUE.)

  if(s==s_g) print*, 'pass'

end program test_string
