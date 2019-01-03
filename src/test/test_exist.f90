program test_exist
  use hdf5_interface
  implicit none

  type(hdf5_file) :: h5f
  integer, allocatable :: iv(:)
  real, allocatable    :: rv(:)


  iv = [1,2,3,4]
  rv = [1.,2.,3.,4.]

  call h5f%open('x.h5', status='new', action='write')

  call h5f%add('/iv', iv)
  call h5f%add('/rv', rv)

  if(h5f%exist('/iv')) print*, 'pass'
  call h5f%delete('/rv')
  if(.not. h5f%exist('/rv')) print*, 'pass'

  call h5f%close(finalize=.TRUE.)



end program test_exist
