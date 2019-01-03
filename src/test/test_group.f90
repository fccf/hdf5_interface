program test_group
  use hdf5_interface
  implicit none

  type(hdf5_file) :: h5f
  integer, allocatable :: im(:,:), im_g(:,:)
  real, allocatable    :: rm(:,:), rm_g(:,:)


  im = reshape([1,2,3,4],shape=[2,2])
  rm = reshape([1.,2.,3.,4.],shape=[2,2])

  call h5f%open('x.h5', status='old', action='write')

  call h5f%add('/matrix/')
  call h5f%open_group('/matrix/')
  call h5f%add('im', im)
  call h5f%add('rm', rm)
  call h5f%close_group()

  call h5f%get('/matrix/im', im_g)
  call h5f%get('/matrix/im', rm_g)

  call h5f%close(finalize=.TRUE.)

  if(all(im_g == im) .and. all(rm_g == rm)) print*, 'pass'

end program test_group
