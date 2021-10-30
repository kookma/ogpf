project('Ogpf', 'fortran')

gnuplot = find_program('gnuplot',
  required: true)

libogpf = library('ogpf', 'src/ogpf.f90',
  fortran_args: '-std=f2008')

libogpf_dep = declare_dependency(
  link_with: libogpf)

executable('demo', 'example/demo.f90',
  dependencies: libogpf_dep)
